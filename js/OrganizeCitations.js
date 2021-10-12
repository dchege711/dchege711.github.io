window.addEventListener("load", organizeCitations);

function organizeCitations() {
    // Get the ordered list element if any.
    const citationElements = document.getElementsByTagName("cite");
    if (!citationElements || citationElements.length === 0) return;

    // The font-size for citations is set globally. Make the list markers share
    // the same font-size. There's currently no way to select the parent via CSS
    // https://stackoverflow.com/questions/1014861/is-there-a-css-parent-selector
    //
    // `citationElements[i].style.fontSize` is empty at this point, so we can't
    // use it. So we reduce the size of the whole thing so as to capture the
    // list marker... (1 of 2)
    for (let i = 0; i < citationElements.length; i++) {
        const citationHolder = citationElements[i].parentElement;
        if (!citationHolder) continue;

        const listItem = citationHolder.parentElement;
        if (!listItem || listItem.tagName.toLowerCase() !== "li") continue;

        const listElement = listItem.parentElement;
        if (!listElement) continue;

        listElement.style.fontSize = "smaller";
    }

    // Match the citation IDs to their position on the list
    let citationIDToDetails = {};
    for (let i = 0; i < citationElements.length; i++) {
        let citationElement = citationElements[i];
        // ... and force the citation element to inherit the size we set above,
        // instead of getting it from the global CSS file. (2 of 2)
        if (citationElement.parentElement &&
            citationElement.parentElement.parentElement.tagName.toLowerCase() === "li") {
            citationElement.parentElement.style.fontSize = "inherit";
        }
        citationIDToDetails[citationElement.id] = {
            hoverText: citationElement.parentElement.innerText
        };
    }

    // Fill in the missing references
    let incompleteAnchors = document.querySelectorAll("span > a");
    for (let i = 0; i < incompleteAnchors.length; i++) {
        let anchor = incompleteAnchors[i];
        let citationID = anchor.href.split("#")[1];
        if (citationIDToDetails[citationID]) {
            anchor.innerText = `${citationID}`;
            anchor.title = citationIDToDetails[citationID].hoverText;
            anchor.parentElement.classList.add("citation-ref-processed");
        }
    }
}
