// anki editor

// suppose only one editor-** exist.
var input = document.getElementsByClassName("editor-input")[0];
var textLayer = document.getElementsByClassName("editor-layer-text")[0]
var cursorLayer = document.getElementsByClassName("editor-layer-cursor")[0]
var currentLine = document.getElementsByClassName("editor-current-line")[0]

input.oninput = (e) => {
    var fontWidth = 10 // unit: px

    currentLine.innerText = currentLine.innerText + e.data
    
    var l1 = cursorLayer.style.left
    var l2 = input.style.left
    if (isNaN(parseInt(l1)) || isNaN(parseInt(l2))) {
        cursorLayer.style.left = fontWidth + "px"
        input.style.left = fontWidth + "px"
    } else {
        cursorLayer.style.left = (parseInt(l1) + fontWidth) + "px"
        input.style.left = (parseInt(l2) + fontWidth) + "px"
    }
}
