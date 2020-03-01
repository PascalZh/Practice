mainUI = document.getElementById('main-ui')
var tabs = mainUI.children
for (i = 0; i < tabs.length; i++) {
    tabs.item(i).classList.add('animated', 'fadeIn')
}

function nextTab()
{
    for (i = 0; i < tabs.length; i++) {
        var tab = tabs.item(i)
        if (!tab.hidden) {
            tab.hidden = true
            tabs.item((i+1) % tabs.length).hidden = false
            break
        }
    }
}
