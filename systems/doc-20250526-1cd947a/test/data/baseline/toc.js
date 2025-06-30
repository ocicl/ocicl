// As the basis I took code from this blog post:
// http://mudox.github.io/post/auto-highlighting-toc/

$(document).ready(function() {
  $(window).scroll(function() {
      $(".page-toc p").removeClass("toc-active");
      currentAnchor().addClass("toc-active");
  })
});

function tocItem(anchor) {
    return $("[href=\"" + anchor + "\"]").parent();
}

function heading(uri) {
    // A uri might contain not only a HTML fragment,
    // but also a path, like:
    // /changelog/#some-version
    //
    // For highlighting TOC item, we need
    // only anchors without path
    if (uri[0] == '#') {
        anchor_id = uri.substr(1);
        return $("[id=" + anchor_id + "]");
    } else {
        return null;
    }
}

var _anchors = null;

function anchors() {
    if (!_anchors) {
        _anchors = $(".page-toc a").map(function() {
            return $(this).attr("href");
        })
    }
    return _anchors;
}

function currentAnchor() {
    var winY = window.pageYOffset;
    var currAnchor = null;
    anchors().each(function() {
        var item = heading(this);

        if (item) {
            var y = item.position().top;
            if (y < winY + window.innerHeight * 0.23) {
                currAnchor = this;
                return;
            }
        }
    })
    return tocItem(currAnchor);
}
