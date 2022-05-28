/**
 * Collect all of the topic sentences, and place them in a comment, right after
 * the table of contents. Inspired by [1]'s recommendation that a reader should
 * be able to skim content using the first sentence of each section.
 *
 * @returns None. Modifes the DOM.
 *
 * [1]: http://www.covingtoninnovations.com/mc/WriteThinkLearn.pdf
 */
function collectTopicSentences() {
    const mainArticleElem = document.getElementById("main-article");
    if (!mainArticleElem) return;

    // For simplicity, we assume that sentences end with a period, question
    // mark, or exclamation mark, followed by whitespace or a quotation mark.
    // Avoid common false-endings like "e.g.", "i.e.", etc. because I rarely
    // use the American comma after abbreviations.
    const endOfSentenceRegex = /(?<!e.g)(?<!i.e)[\.?!][\s^”]/;

    let topicSentencesHTML = "<p>";
    for (let elem of mainArticleElem.querySelectorAll("H2, ARTICLE > P")) {
        // H2 headers are the highest inside the main-article div. H1 is
        // reserved for the page's title. Because we don't want nesting inside
        // `topicSentencesHTML`, we demarcate `<p>` elements using H2 elements.
        if (elem.tagName === "H2") {
            topicSentencesHTML += "</p>";
            continue;
        }

        const found = elem.innerText.match(endOfSentenceRegex);
        if (!found) continue;

        const endOfTopicSentenceIdx = found.index + found[0].length;
        const firstSentence = elem.innerText.substring(0, endOfTopicSentenceIdx);

        topicSentencesHTML += `${firstSentence} `;
    }

    if (topicSentencesHTML === "<p>") {
        // Then we did not find any eligible topic sentences.
        return;
    } else {
        topicSentencesHTML += `</p>`;
    }

    const readersNote = `<hr /><p><i>This block was automatically generated from
    the first sentence of each paragraph. Inspired by the idea that <a
    href="/writing/the-writing-process/#SkimViaTopicSentences">a reader should
    be able to skim by reading the first sentence of each paragraph.</a></i></p>`;

    mainArticleElem.insertAdjacentHTML(
        "beforebegin",
        `<div class="comment-holder"><div class="comment">
            ${topicSentencesHTML}${readersNote}
        </div></div>`);
}

window.addEventListener("load", collectTopicSentences);
