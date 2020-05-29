window.onload = organizeCitations;

function organizeCitations() {
    // Get the ordered list element if any.
    const citationElements = document.getElementsByTagName("cite");
    if (!citationElements || citationElements.length === 0) return;

    const citationHolder = citationElements[0].parentElement;
    if (!citationHolder) return;

    const listItem = citationHolder.parentElement;
    if (!listItem || listItem.tagName.toLowerCase() !== "li") return;

    const listElement = listItem.parentElement;
    if (!listElement) return;

    // Reduce the font-size of the citations. There's currently no way to select
    // the parent via CSS.
    // https://stackoverflow.com/questions/1014861/is-there-a-css-parent-selector
    listElement.style.fontSize = "smaller";

    // Match the citation IDs to their position on the list
    let citationIDToDetails = {};
    for (let i = 0; i < listElement.children.length; i++) {
        let citationElement = listElement.children[i].getElementsByTagName("cite")[0];
        citationIDToDetails[citationElement.id] = {
            numToDisplay: i + 1,
            hoverText: citationElement.parentElement.innerText
        };
    }

    // Fill in the missing references
    let incompleteAnchors = document.querySelectorAll("sup > a");
    for (let i = 0; i < incompleteAnchors.length; i++) {
        let anchor = incompleteAnchors[i];
        let citationID = anchor.href.split("#")[1];
        if (citationIDToDetails[citationID]) {
            anchor.innerText = `[${citationIDToDetails[citationID].numToDisplay}]`;
            anchor.title = citationIDToDetails[citationID].hoverText;
        }        
    }
}