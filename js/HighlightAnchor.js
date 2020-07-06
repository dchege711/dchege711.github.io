window.addEventListener("load", highlightAnchor);

/**
 * @description If the URL has an anchor, highlight its block so that the
 * relevant section is more apparent to the user. Browsers tend to scroll to the
 * anchor. Highlighting the text improves on this.
 * 
 * @param {Event} ev The load event.
 */
function highlightAnchor(ev) {
    const fragmentID = (new URL(window.location.href)).hash;
    if (!fragmentID) return;

    let elem = document.querySelector(fragmentID);
    if (!elem) return;

    // Underline the element so that it's visible in the selection.
    elem.style.setProperty("border-bottom", "rgb(24, 179, 179) solid thick");

    // Highlight the first ancestor that is a block element, i.e. one that
    // generates a newline before and after itself when being laid out.
    while (elem.parentElement) {
        let style = window.getComputedStyle(elem, "display");
        if (style.getPropertyValue("display") === "block") break;
        elem = elem.parentElement;
    }
    elem.style.backgroundColor = "yellow";
}