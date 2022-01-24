window.addEventListener("load", organizeCitations);
window.addEventListener("load", inlineDefinitions);

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

        const citationIconClass = citationElement.parentElement.getAttribute(
            "citation-icon-class");

        const citedByCount = citationElement.parentElement.getAttribute( "cited-by-count")

        citationIDToDetails[citationElement.id] = {
            hoverText: citationElement.parentElement.innerText,
            iconHTML: (citationIconClass
                ? `<i class="${citationIconClass}" style="margin-left:3px; margin-right:3px;" aria-hidden="true"></i>`
                : ""),
            citationRefIds: [],
            citedByCountHTML: (citedByCount ? `<sup>${citedByCount}</sup>` : "")
        };
    }

    // Fill in the missing references
    let incompleteAnchors = document.querySelectorAll("span > a");
    for (let i = 0; i < incompleteAnchors.length; i++) {
        let anchor = incompleteAnchors[i];
        let citationID = anchor.href.split("#")[1];
        if (citationIDToDetails[citationID]) {
            anchor.innerHTML =
                `${citationID}${citationIDToDetails[citationID].iconHTML}${citationIDToDetails[citationID].citedByCountHTML}`;
            anchor.title = citationIDToDetails[citationID].hoverText;
            anchor.parentElement.classList.add("citation-ref-processed");
            const citationRefId = `citation-ref-${i}`;
            anchor.parentElement.id = citationRefId;
            citationIDToDetails[citationID].citationRefIds.push(citationRefId);
        }
    }

    // Add references to go back to the content from a citation.
    for (let i = 0; i < citationElements.length; i++) {
        const citationElement = citationElements[i];
        for (let citationRefId of citationIDToDetails[citationElement.id].citationRefIds) {
            let citationRefElem = document.createElement("a");
            citationRefElem.href = `#${citationRefId}`;
            citationRefElem.innerText = "^";
            citationRefElem.title = `Jump up to ${citationRefId}`;
            citationRefElem.classList.add("citation-ref-backlink");
            citationElement.parentElement.insertAdjacentElement("beforeend", citationRefElem);
        }
    }
}

function inlineDefinitions() {
    // Build a lookup table for the definitions.
    const descListElements = document.getElementsByTagName("dl");
    if (!descListElements || descListElements.length === 0) return;

    const termIDToDefinitionText = {};
    for (let i = 0; i < descListElements.length; i++) {
        const elem = descListElements[i];
        let termID = elem.getElementsByTagName("dt")[0].id;
        let definitionText = elem.innerText;
        termIDToDefinitionText[termID] = definitionText;
    }

    // Populate the title attribute of all anchors that reference definitions.
    for (const [termID, definitionText] of Object.entries(termIDToDefinitionText)) {
        const matchingAnchorElements = document.querySelectorAll(`a[href="#${termID}"]`);
        for (let i = 0; i < matchingAnchorElements.length; i++) {
            matchingAnchorElements[i].title = definitionText;
        }
    }
}
