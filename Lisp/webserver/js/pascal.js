var xmlhttp;
var screen_shot_btn = document.getElementById("screen_shot_btn");
var screen_shot_img = document.getElementById("screen_img");
var old = screen_shot_btn.innerHTML;
var screen_shot_tbody = document.getElementById("screen_shot_list");
var flagGetList = false
xmlhttp=new XMLHttpRequest();

function onCBScreenShot()
{
  if (xmlhttp.readyState==4 && xmlhttp.status==200)
  {
    var str = xmlhttp.responseText;
    if (str.search(/success/i) == 0) {
      document.getElementById("screen_img").src = "";
      setTimeout(function(){ 
        document.getElementById("screen_img").src = "/tmp/scrot.png" + "?=" + Math.random();
        screen_shot_btn.disabled=false;
        screen_shot_btn.innerHTML=old;
        flagGetList = false;
      }, 1000)
    } else {
      alert(xmlhttp.responseText);
    }
  }
}

function onTestAjax()
{
  var xmlhttp;
  xmlhttp=new XMLHttpRequest();
  xmlhttp.onreadystatechange=function()
  {
    if (xmlhttp.readyState==4 && xmlhttp.status==200)
    {
      //alert(xmlhttp.responseText);
      document.getElementById("test_ajax").innerHTML=xmlhttp.responseText;
    }
  }
  xmlhttp.open("GET","/test/test_ajax",true);
  xmlhttp.send();
}

function onScreenShot() {
  //var btn = document.getElementById("screen_shot_btn");
  //alert(btn);
  screen_shot_btn.disabled=true;
  screen_shot_btn.innerHTML="截图中...";

  xmlhttp.onreadystatechange = onCBScreenShot;
  xmlhttp.open("GET","/test/screen_shot",true);
  xmlhttp.send();
}

function onExpandScreenShotList()
{
  xmlhttp.onreadystatechange = onCBESSL;
  xmlhttp.open("GET","/test/screen_shot_get_list", true)
  xmlhttp.send();
}

// 由于是bootstrap的collapse按钮，所以按一次这个函数会被调用四次
//var count = 0;
function onCBESSL()
{
  if (xmlhttp.readyState==4 && xmlhttp.status==200 && (! flagGetList)) {
    var lst = xmlhttp.responseText.split('|').map(function(tt){return "<tr><td name=\"td1\">"+tt+"</td></tr>";});
    var str = lst.join("");
    screen_shot_tbody.innerHTML = str;
    var tds = document.getElementsByName('td1');
    tds.forEach(function(td){
      td.onclick = function() {
        screen_shot_img.src = "/test/screen_shot_get/" + td.innerHTML;
        tds.forEach(function(td_){td_.style.backgroundColor = "";});
        td.style = "background-color: #808080";
        };
      });
    flagGetList = true;
  } else {
    //count = count + 1;
    //alert(count);
  }
}
function arrayBufferToBase64( buffer ) {
    var binary = '';
    var bytes = new Uint8Array( buffer );
    var len = bytes.byteLength;
    for (var i = 0; i < len; i++) {
        binary += String.fromCharCode( bytes[ i ] );
    }
    return window.btoa( binary );
}
