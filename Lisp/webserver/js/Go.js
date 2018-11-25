var FiveChesscanvas = document.getElementById('ChessCanvas');
var context = FiveChesscanvas.getContext('2d');
// has_stone 标记棋盘上是否有棋子
// 没有:0白子:1黑子:2
var flag_none = 0; var flag_white = 1; var flag_black = 2;
var has_stone = [];
var is_me = true;
var is_me_white = false;
var is_over = false;
var chess_http = new XMLHttpRequest();

// 用来把字母横坐标映射成数
var map_a2n = 'ABCDEFGHJKLMNOPQRST'.split('')

DrawChessBoard();
for(var i=0; i<19; i++) {
  has_stone[i] = [];
  for(var j=0; j<19; j++) {
    has_stone[i][j] = 0;
  }
}

function isWhite() {
  return is_me_white? is_me: !is_me;
}

function DrawChessBoard() {
  context.beginPath();
  for(var i=0; i<19; i++) {
    context.moveTo(15+30, 15+30*i+30);
    context.lineTo(15+30*18+30, 15+30*i+30);
    context.moveTo(15+30*i+30, 15+30);
    context.lineTo(15+30*i+30, 15+30*18+30);
  }
  context.strokeStyle = "#666";
  context.stroke();
  context.closePath();

  // sign
  context.beginPath();
  drawSign(3, 3, context); drawSign(3, 9, context); drawSign(3, 15, context)
  drawSign(9, 3, context); drawSign(9, 9, context); drawSign(9, 15, context)
  drawSign(15, 3, context); drawSign(15, 9, context); drawSign(15, 15, context)
  context.lineWidth = 2; context.strokeStyle = "#000";
  context.stroke();
  context.closePath();

  // coordinates
  drawCoordinates(context);
}

function drawCoordinates(context) {
  context.font = '10px Verdana';
  for(var i=0; i<19; i++) {
    context.fillText(map_a2n[i], 42+i*30, 25);
    context.fillText(map_a2n[i], 42+i*30, 70+30*18);
    context.fillText(19-i, 13, 48+i*30);
    context.fillText(19-i, 70+30*18, 48+i*30);
  }
}

function drawSign(i, j, context_) {
  context_.moveTo(15+30*i-15+30, 15+30*j+30)
  context_.lineTo(15+30*i+15+30, 15+30*j+30)
  context_.moveTo(15+30*i+30, 15+30*j-15+30)
  context_.lineTo(15+30*i+30, 15+30*j+15+30)
}

function oneStep(i, j) {
  context.beginPath();
  context.arc(30*i+15+30, 30*j+15+30, 13, 0, Math.PI*2);
  var gradient = context.createRadialGradient(30*i+15+30, 30*j+15+30, 13, 30*i+15+30, 30*j+15+30, 0);
  if(isWhite()) {
    gradient.addColorStop(0, "#b1b1b1");
    gradient.addColorStop(1, "#f9f9f9");
  }
  else {
    gradient.addColorStop(0, "#0a0a0a");
    gradient.addColorStop(1, "#636766");
  }
  context.fillStyle = gradient;
  context.fill();
  context.closePath();
}

FiveChesscanvas.onclick = function(e) {
  var x = e.offsetX;
  var y = e.offsetY;
  var i = Math.floor(x/30)-1;
  var j = Math.floor(y/30)-1;
  if(i>=0 && j>=0 &&
    has_stone[i][j] == flag_none &&
    is_me) {

    oneStep(i, j);

    var url_ = "/Go/play/b"
    if (isWhite()) {
      url_ = "/Go/play/w"
    }

    has_stone[i][j] = isWhite()? flag_white: flag_black;

    url_ += map_a2n[i] + (19 - j).toString();
    console.log("onclick: get: url:" + url_);
    chess_http.open("GET", url_);
    chess_http.send();
    is_me = false;
  }
}

var reMove = /[ABCDEFGHJKLMNOPQRST][1-19]/
chess_http.onreadystatechange = function() {
  if (chess_http.readyState==4 && chess_http.status==200)
  {
    var i, j;
    var ret = chess_http.responseText;
    console.log("receive from /Go:" + ret);

    if (reMove.test(ret)) {
      i = map_a2n.findIndex(function(v) {
        return v==ret.slice(0,1)});
      j = 19 - Number(ret.slice(1));
      has_stone[i][j] = isWhite()? flag_white: flag_black;
      oneStep(i, j);
      // 恢复点击，继续下棋
      is_me = true;

    } else if (/start ok/.test(ret)) {
      ;
    }
  }
}
