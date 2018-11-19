var screen_shot_btn = document.getElementById("screen_shot_btn");
var screen_shot_img = document.getElementById("screen_img");

var old;
if (screen_shot_btn) {
  old = screen_shot_btn.innerHTML;
}
var screen_shot_tbody = document.getElementById("screen_shot_list");
var flagGetList = false

var xmlhttp;
xmlhttp=new XMLHttpRequest();

// 登录后删除Login按钮
if ($('#admin_img').length != 0) {
  $('#my_nav_login').hide();
} else {
  $('#my_nav_login').show();
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

function onCBScreenShot()
{
  if (xmlhttp.readyState==4 && xmlhttp.status==200)
  {
    var str = xmlhttp.responseText;
    if (str.search(/success/i) == 0) {
      setTimeout(function(){ 
        hideLoad();
        screen_shot_img.src = "/tmp/scrot.png" + "?=" + Math.random();
        screen_shot_btn.disabled=false;
        screen_shot_btn.innerHTML=old;
        flagGetList = false;
      }, 1000)
    } else {
      alert(xmlhttp.responseText);
    }
  }
}

function onScreenShot() {
  //var btn = document.getElementById("screen_shot_btn");
  //alert(btn);
  screen_shot_btn.disabled=true;
  screen_shot_btn.innerHTML="截图中...";
  showLoad();

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
        showLoad();
        screen_shot_img.src = "/test/screen_shot_get/" + td.innerHTML;
        hideLoad();
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

function showLoad() {
  //var tmp = document.createElement("div");
  //tmp.innerHTML = "<div id= \"loading\" class=\"loader\"><div class=\"face\"><div class=\"circle\"></div></div><div class=\"face\"><div class=\"circle\"></div></div></div>"
  screen_shot_img.src = "/images/loading.gif";
  //screen_shot_img.hidden = true;
  //screen_shot_img.after(tmp.childNodes[0]);
}

function hideLoad() {
  //setTimeout(function(){
  //screen_shot_img.hidden = false;
  //$('#loading').remove();
  //}, 400);
}

function onLogout() {
  //document.cookie.split(";").forEach(function(c) { document.cookie = c.replace(/^ +/, "").replace(/=.*/, "=;expires=" + new Date().toUTCString() + ";path=/"); });
    var cookies = document.cookie.split(";");

    for (var i = 0; i < cookies.length; i++) {
        var cookie = cookies[i];
        var eqPos = cookie.indexOf("=");
        var name = eqPos > -1 ? cookie.substr(0, eqPos) : cookie;
      document.cookie = name + "=;expires=Thu, 01 Jan 1970 00:00:00 GMT";
    }
  (function () {
    var cookies = document.cookie.split("; ");
    for (var c = 0; c < cookies.length; c++) {
      var d = window.location.hostname.split(".");
      while (d.length > 0) {
        var cookieBase = encodeURIComponent(cookies[c].split(";")[0].split("=")[0]) + '=; expires=Thu, 01-Jan-1970 00:00:01 GMT; domain=' + d.join('.') + ' ;path=';
        var p = location.pathname.split('/');
        document.cookie = cookieBase + '/';
        while (p.length > 0) {
          document.cookie = cookieBase + p.join('/');
          p.pop();
        };
        d.shift();
      }
    }
  })();
  window.location.reload();
}
