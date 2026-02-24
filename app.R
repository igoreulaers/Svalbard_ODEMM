library(shiny)
library(htmlwidgets)
library(networkD3)
library(rsvg)
library(base64enc)
source("settlement_sankey.R")
source("wilderness_sankey.R")

EXCEL_PATH <- "data.xlsx"

# ── Label-positioning JS ─────────────────────────────────────────────────────
# After the sankey renders, reposition node labels so that:
#   left-column  nodes → label sits to the LEFT  of the bar (text-anchor: end)
#   right-column nodes → label sits to the RIGHT of the bar (text-anchor: start)
#   middle nodes       → keep default behaviour
label_js <- "
function(el, x) {
  function fixLabels() {
    var svg = el.querySelector('svg');
    if (!svg) return false;

    var nodeGs = Array.from(svg.querySelectorAll('g.node'));
    if (!nodeGs.length) return false;

    // Allow labels to render outside the SVG boundary instead of being clipped
    svg.style.overflow = 'visible';
    el.style.overflow  = 'visible';

    // Read actual nodeWidth from the rendered rect rather than hardcoding
    var firstNodeRect = svg.querySelector('g.node rect');
    var NODE_WIDTH = firstNodeRect ? parseFloat(firstNodeRect.getAttribute('width') || 20) : 20;
    var PAD_LEFT   = 26;
    var PAD_RIGHT  = 26;

    function getNodeX(g) {
      var tf = g.getAttribute('transform') || '';
      var m  = tf.match(/translate\\(([\\d.e+-]+)[,\\s]+([\\d.e+-]+)\\)/);
      return m ? parseFloat(m[1]) : null;
    }

    var allX = nodeGs.map(getNodeX).filter(function(v) { return v !== null; });
    if (!allX.length) return false;

    var uniqueX = allX
      .map(function(v) { return Math.round(v); })
      .filter(function(v, i, a) { return a.indexOf(v) === i; })
      .sort(function(a, b) { return a - b; });

    if (uniqueX.length < 2) return false;

    var leftColX  = uniqueX[0];
    var rightColX = uniqueX[uniqueX.length - 1];
    var THRESH    = 2;

    nodeGs.forEach(function(g) {
      var text = g.querySelector('text');
      if (!text) return;

      var nx = Math.round(getNodeX(g));

      if (Math.abs(nx - leftColX) <= THRESH) {
        text.setAttribute('x',           -PAD_LEFT);
        text.setAttribute('text-anchor', 'end');
        text.setAttribute('dy',          '0.35em');
      } else if (Math.abs(nx - rightColX) <= THRESH) {
        text.setAttribute('x',           NODE_WIDTH + PAD_RIGHT);
        text.setAttribute('text-anchor', 'start');
        text.setAttribute('dy',          '0.35em');
      }
    });

    return true;
  }

  var tries = 0;
  var timer = setInterval(function() {
    tries++;
    if (fixLabels() || tries >= 60) clearInterval(timer);
  }, 150);
}
"

# ── Highlight / interaction JS ───────────────────────────────────────────────
highlight_js <- "
function(el, x) {

  function getLinkInfo(pathEl) {
    var title = pathEl.querySelector('title');
    if (!title) return null;
    var parts = title.textContent.split('\\n')[0].split(' \u2192 ');
    if (parts.length !== 2) parts = title.textContent.split('\\n')[0].split(' -> ');
    if (parts.length !== 2) return null;
    return { source: parts[0].trim(), target: parts[1].trim() };
  }

  function bindWhenReady() {
    var svg = el.querySelector('svg');
    if (!svg) return false;

    var paths = Array.from(svg.querySelectorAll('path'));
    if (!paths.length) return false;

    var testInfo = null;
    for (var i = 0; i < paths.length; i++) {
      testInfo = getLinkInfo(paths[i]);
      if (testInfo) break;
    }
    if (!testInfo) return false;

    // Read actual nodeWidth from the first node rect rather than hardcoding
    var firstNodeRect = svg.querySelector('g.node rect');
    var NODE_WIDTH = firstNodeRect ? parseFloat(firstNodeRect.getAttribute('width') || 20) : 20;

    var nodeToLinks = {};
    paths.forEach(function(p) {
      var info = getLinkInfo(p);
      if (!info) return;
      [info.source, info.target].forEach(function(nm) {
        if (!nodeToLinks[nm]) nodeToLinks[nm] = [];
        nodeToLinks[nm].push(p);
      });
    });

    var nodeData = {};
    paths.forEach(function(p) {
      var info = getLinkInfo(p);
      if (!info) return;
      var d = p.getAttribute('d') || '';
      var mMatch = d.match(/M\\s*([\\d.e+-]+)[,\\s]+([\\d.e+-]+)/);
      if (mMatch) {
        var nm = info.source;
        if (!nodeData[nm]) nodeData[nm] = { xs: [], ys: [] };
        nodeData[nm].xs.push(parseFloat(mMatch[1]));
        nodeData[nm].ys.push(parseFloat(mMatch[2]));
      }
      var numPairs = [];
      var re = /([\\d.e+-]+)[,\\s]+([\\d.e+-]+)/g;
      var m;
      while ((m = re.exec(d)) !== null) {
        numPairs.push([parseFloat(m[1]), parseFloat(m[2])]);
      }
      if (numPairs.length >= 2) {
        var endPair = numPairs[numPairs.length - 1];
        var tnm = info.target;
        if (!nodeData[tnm]) nodeData[tnm] = { xs: [], ys: [] };
        nodeData[tnm].xs.push(endPair[0]);
        nodeData[tnm].ys.push(endPair[1]);
      }
    });

    var old = svg.querySelector('#hl-hit-targets');
    if (old) old.parentNode.removeChild(old);

    var svgNS  = 'http://www.w3.org/2000/svg';
    var hitGroup = document.createElementNS(svgNS, 'g');
    hitGroup.setAttribute('id', 'hl-hit-targets');

    var svgBox = svg.getBoundingClientRect();
    var svgW   = svgBox.width || parseFloat(svg.getAttribute('width')) || 1000;

    var topG = svg.querySelector('g');

    Object.keys(nodeData).forEach(function(nm) {
      var xs   = nodeData[nm].xs;
      var ys   = nodeData[nm].ys;
      var avgX = xs.reduce(function(a, b) { return a + b; }, 0) / xs.length;
      var minY = Math.min.apply(null, ys);
      var maxY = Math.max.apply(null, ys);
      var pad  = 8;

      // avgX is the path endpoint touching the node edge:
      //   left nodes:   path starts at RIGHT edge of bar -> bar starts at avgX - NODE_WIDTH
      //   right nodes:  path ends   at LEFT  edge of bar -> bar starts at avgX
      //   middle nodes: mix of both  -> centre on avgX
      var svgMidX = (svgBox.width || 1000) / 2;
      var rectX;
      if      (avgX < svgMidX * 0.4)  rectX = avgX - NODE_WIDTH;  // left column
      else if (avgX > svgMidX * 1.6)  rectX = avgX;               // right column
      else                             rectX = avgX - NODE_WIDTH / 2; // middle

      var rect = document.createElementNS(svgNS, 'rect');
      rect.setAttribute('x',      rectX);
      rect.setAttribute('y',      minY - pad);
      rect.setAttribute('width',  NODE_WIDTH);
      rect.setAttribute('height', Math.max(maxY - minY + pad * 2, 20));
      rect.setAttribute('fill',   'transparent');
      rect.style.cursor = 'pointer';
      rect._nodeName = nm;
      hitGroup.appendChild(rect);
    });

    var container = topG || svg;
    container.appendChild(hitGroup);

    paths = paths.map(function(p) {
      var clone = p.cloneNode(true);
      p.parentNode.replaceChild(clone, p);
      return clone;
    });

    var defaultOpacity = {};
    paths.forEach(function(p, i) {
      var computed = window.getComputedStyle(p).strokeOpacity;
      var inline   = p.style.strokeOpacity || p.getAttribute('stroke-opacity') || computed || '0.4';
      defaultOpacity[i] = inline;
    });

    nodeToLinks = {};
    paths.forEach(function(p) {
      var info = getLinkInfo(p);
      if (!info) return;
      [info.source, info.target].forEach(function(nm) {
        if (!nodeToLinks[nm]) nodeToLinks[nm] = [];
        nodeToLinks[nm].push(p);
      });
    });

    function restoreAll() {
      paths.forEach(function(p, i) {
        p.style.strokeOpacity = defaultOpacity[i];
        p.style.opacity       = '';
      });
    }

    function highlightPaths(pathList) {
      pathList.forEach(function(p) {
        p.style.strokeOpacity = '1';
        p.style.opacity       = '';
      });
    }

    var locked = null;
    function doReset()      { locked = null; restoreAll(); }
    function activateNode(nm) {
      if (locked === nm)    { doReset(); return; }
      locked = nm; restoreAll(); highlightPaths(nodeToLinks[nm] || []);
    }

    window.sankeyReset = doReset;

    hitGroup.querySelectorAll('rect').forEach(function(r) {
      r.addEventListener('click', function(evt) {
        evt.stopPropagation(); activateNode(r._nodeName);
      });
    });

    paths.forEach(function(p) {
      p.style.cursor = 'pointer';
      p.addEventListener('click', function(evt) {
        evt.stopPropagation();
        var info = getLinkInfo(p);
        if (!info) return;
        if (locked === info.source || locked === info.target) doReset();
        else activateNode(info.source);
      });
    });

    if (svg._bgHandler) svg.removeEventListener('click', svg._bgHandler);
    svg._bgHandler = function() { doReset(); };
    svg.addEventListener('click', svg._bgHandler);

    console.log('[highlight] bound -', paths.length, 'links,', Object.keys(nodeData).length, 'nodes');
    return true;
  }

  var tries = 0;
  var timer = setInterval(function() {
    tries++;
    if (bindWhenReady() || tries >= 60) clearInterval(timer);
  }, 150);
}
"

# ── SVG serialisation + PDF download JS ─────────────────────────────────────
svg_js <- "
(function() {
  if (window.__sankeyHandlersRegistered) return;
  window.__sankeyHandlersRegistered = true;

  Shiny.addCustomMessageHandler('getSvgForDownload', function(msg) {
    var container = document.getElementById('sankey_out');
    var svgEl = container ? container.querySelector('svg') : document.querySelector('svg');
    if (!svgEl) { Shiny.setInputValue('svg_content', '', {priority: 'event'}); return; }

    var clone = svgEl.cloneNode(true);
    var hitGroup = clone.querySelector('#hl-hit-targets');
    if (hitGroup) hitGroup.parentNode.removeChild(hitGroup);

    var bb = svgEl.getBoundingClientRect();
    clone.setAttribute('width',  bb.width  || svgEl.getAttribute('width')  || 1200);
    clone.setAttribute('height', bb.height || svgEl.getAttribute('height') || 700);

    var livePaths = svgEl.querySelectorAll('path');
    clone.querySelectorAll('path').forEach(function(p, i) {
      if (!livePaths[i]) return;
      var cs = window.getComputedStyle(livePaths[i]);
      p.setAttribute('stroke',         cs.stroke);
      p.setAttribute('stroke-opacity', cs.strokeOpacity);
      p.setAttribute('stroke-width',   cs.strokeWidth);
      p.setAttribute('fill',           'none');
    });
    var liveTexts = svgEl.querySelectorAll('text');
    clone.querySelectorAll('text').forEach(function(t, i) {
      if (!liveTexts[i]) return;
      var cs = window.getComputedStyle(liveTexts[i]);
      t.setAttribute('font-size',   cs.fontSize);
      t.setAttribute('font-family', cs.fontFamily);
      t.setAttribute('fill',        cs.fill);
    });
    var liveRects = svgEl.querySelectorAll('rect');
    clone.querySelectorAll('rect').forEach(function(r, i) {
      if (!liveRects[i]) return;
      var cs = window.getComputedStyle(liveRects[i]);
      r.setAttribute('fill',         cs.fill);
      r.setAttribute('fill-opacity', cs.fillOpacity || '1');
      r.setAttribute('stroke',       cs.stroke);
    });

    Shiny.setInputValue('svg_content', new XMLSerializer().serializeToString(clone), {priority: 'event'});
  });

  Shiny.addCustomMessageHandler('downloadPdf', function(msg) {
    var byteChars = atob(msg.b64);
    var byteNums  = new Uint8Array(byteChars.length);
    for (var i = 0; i < byteChars.length; i++) byteNums[i] = byteChars.charCodeAt(i);
    var blob = new Blob([byteNums], {type: 'application/pdf'});
    var url  = URL.createObjectURL(blob);
    var a    = document.createElement('a');
    a.href = url; a.download = msg.filename;
    document.body.appendChild(a); a.click(); document.body.removeChild(a);
    setTimeout(function() { URL.revokeObjectURL(url); }, 10000);
  });

  Shiny.addCustomMessageHandler('resetHighlight', function(msg) {
    if (typeof window.sankeyReset === 'function') window.sankeyReset();
  });
})();
"

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("ODEMM - Svalbard fjords"),
  
  tags$head(tags$script(HTML(svg_js))),
  
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
        tags$strong("Fjord type:"),
        div(
          style = "margin-bottom: 0;",
          tags$style(HTML(".form-group { margin-bottom: 0 !important; }
                           #fjord_state + .selectize-control .selectize-input { 
                             padding-top: 6px; padding-bottom: 6px; 
                           }")),
          selectInput("fjord_state", label = NULL,
                      choices  = c("Settlement" = "settlement",
                                   "Wilderness" = "wilderness"),
                      selected = "settlement", width = "250px")
        ),
        div(style = "margin-bottom: 0;",
            actionButton("download_btn", "Download graph (PDF)",
                         style = "height: 38px; vertical-align: middle;")),
        div(style = "margin-bottom: 0;",
            actionButton("reset_btn", "Reset highlight",
                         style = "height: 38px; vertical-align: middle; background-color: #f8f9fa; border: 1px solid #ced4da;"))
      )
    )
  ),
  
  fluidRow(
    column(12,
           p("Click a node or link to highlight connections. Click again or press 'Reset highlight' to clear.",
             style = "color: #888; font-size: 0.85em; margin: 4px 0;"))
  ),
  
  hr(),
  
  fluidRow(
    column(width = 12,
           div(style = "padding-left: 150px; padding-right: 150px; overflow: visible;",
               sankeyNetworkOutput("sankey_out", width = "100%", height = "600px")))
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  output$sankey_out <- renderSankeyNetwork({
    w <- if (input$fjord_state == "settlement") {
      settlement_sankey(EXCEL_PATH)
    } else {
      wilderness_sankey(EXCEL_PATH)
    }
    # Apply both JS callbacks: labels first, then interaction binding
    w <- htmlwidgets::onRender(w, label_js)
    w <- htmlwidgets::onRender(w, highlight_js)
    w
  })
  
  observeEvent(input$reset_btn, {
    session$sendCustomMessage("resetHighlight", list())
  })
  
  observeEvent(input$download_btn, {
    session$sendCustomMessage("getSvgForDownload", list())
  })
  
  observeEvent(input$svg_content, {
    req(nchar(input$svg_content) > 0)
    svg_str <- input$svg_content
    tmp_svg <- tempfile(fileext = ".svg")
    tmp_pdf <- tempfile(fileext = ".pdf")
    writeLines(svg_str, tmp_svg, useBytes = TRUE)
    rsvg::rsvg_pdf(tmp_svg, file = tmp_pdf)
    b64      <- base64enc::base64encode(tmp_pdf)
    filename <- paste0("sankey_", isolate(input$fjord_state), ".pdf")
    session$sendCustomMessage("downloadPdf", list(b64 = b64, filename = filename))
  })
}

shinyApp(ui, server)