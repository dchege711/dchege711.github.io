window.addEventListener("load", highlightAnchor);
window.addEventListener("hashchange", highlightAnchor);

let HighlightAnchorState = {
    /** The timeout ID for the pending `removeHighlight` run. */
    timeoutID: null,

    /** The element that matched the `window.location.hash`. */
    fragmentElem: null,

    /**
     * The first parent (including self) of `fragmentElem` that has a blocking
     * display CSS property.
     */
    firstblockingParentElem: null,
};


/**
 * @description If the URL has an anchor, highlight its block so that the
 * relevant section is more apparent to the user. Browsers tend to scroll to the
 * anchor. Highlighting the text improves on this.
 *
 * @param {Event} ev The load event.
 */
function highlightAnchor(ev) {
    const fragmentID = window.location.hash;
    if (!fragmentID) return;

    if (HighlightAnchorState.timeoutID) {
        clearTimeout(HighlightAnchorState.timeoutID);
        removeHighlight();
    }

    HighlightAnchorState.fragmentElem = document.querySelector(fragmentID);
    if (!HighlightAnchorState.fragmentElem) return;

    // Underline the element so that it's visible in the selection.
    HighlightAnchorState.fragmentElem.classList.add("highlight-underline");

    // Highlight the first ancestor that is a block element, i.e. one that
    // generates a newline before and after itself when being laid out.
    HighlightAnchorState.firstblockingParentElem = HighlightAnchorState.fragmentElem;
    while (HighlightAnchorState.firstblockingParentElem.parentElement) {
        const style = window.getComputedStyle(
            HighlightAnchorState.firstblockingParentElem);
        const isVisible = (HighlightAnchorState.firstblockingParentElem.clientWidth > 0) &&
            (HighlightAnchorState.firstblockingParentElem.clientHeight > 0);

        // For DT elements, we want to bubble up until the DL element so that we
        // can highlight the whole description list.
        const isEligible = HighlightAnchorState.firstblockingParentElem.tagName !== "DT";

        if (style.getPropertyValue("display") === "block" && isVisible && isEligible) break;

        HighlightAnchorState.firstblockingParentElem =
            HighlightAnchorState.firstblockingParentElem.parentElement;
    }
    HighlightAnchorState.firstblockingParentElem.classList.add("highlight-background");
    HighlightAnchorState.timeoutID = setTimeout(removeHighlight, 3000);
}

/**
 * Remove the highlight CSS set in `highlightAnchor`. This is necessary so that
 * we don't have multiple elements with selections, because that'd be confusing.
 */
function removeHighlight() {
    HighlightAnchorState.fragmentElem.classList.remove("highlight-underline");
    HighlightAnchorState.firstblockingParentElem.classList.remove("highlight-background");
}
