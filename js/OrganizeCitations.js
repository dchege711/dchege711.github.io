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

    // Match the citation IDs to their position on the list
    let citationIDToNum = {};
    for (let i = 0; i < listElement.children.length; i++) {
        let citationElement = listElement.children[i].getElementsByTagName("cite")[0];
        citationIDToNum[citationElement.id] = i + 1;
    }

    // Fill in the missing references
    let incompleteAnchors = document.querySelectorAll("sup > a");
    for (let i = 0; i < incompleteAnchors.length; i++) {
        let anchor = incompleteAnchors[i];
        let citationID = anchor.href.split("#")[1];
        if (citationIDToNum[citationID]) {
            anchor.innerText = `[${citationIDToNum[citationID]}]`;
        }        
    }
}