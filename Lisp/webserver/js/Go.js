let FiveChesscanvas = document.getElementById('ChessCanvas');
let context = FiveChesscanvas.getContext('2d');
// has_stone 标记棋盘上是否有棋子
// 没有:0白子:1黑子:2
const flag_none = 0; const flag_white = 1; const flag_black = 2;
let has_stone = [];
let is_me = false;
let is_me_white = false;
//let is_over = false;
let chess_http = new XMLHttpRequest();
// 用来把字母横坐标映射成数
let map_a2n = 'ABCDEFGHJKLMNOPQRST'.split('');

drawChessBoard();
for(let i=0; i<19; i++) {
  has_stone[i] = [];
  for(let j=0; j<19; j++) {
    has_stone[i][j] = 0;
  }
}

function onChessStart() {
  clearBoard();
  let leelaz = $('#leelaz')[0].checked;
  let AGo = $('#AGo')[0].checked;
  let PvC = $('#PvC')[0].checked;
  let PvP = $('#CvC')[0].checked;
  if (leelaz) {
    let player_id = getPlayerId();
    chess_http.open('GET', `/Go/start/${player_id}/leelaz`, true);
    chess_http.send();
  } else if (AGo) {
    let player_id = getPlayerId();
    chess_http.open('GET', `/Go/start/${player_id}/AGo`, true);
    chess_http.send();
  }
}

function clearBoard() {
  for(let i=0; i<19; i++) {
    for(let j=0; j<19; j++) {
      has_stone[i][j] = 0;
    }
  }
  // 清空画布
  context.clearRect(0,0,630,630);
  drawChessBoard();
}

function getPlayerId() {
  return 'foo';
}

function onChessStop() {
  chess_http.open('GET', '/Go/stop', true);
  chess_http.send();
}

function onChessRestart() {
  chess_http.open('GET', '/Go/clear_board', true);
  chess_http.send();
}

function isWhite() {
  return is_me_white? is_me: !is_me;
}

function drawChessBoard() {
  context.beginPath();
  for(let i=0; i<19; i++) {
    context.moveTo(15+30, 15+30*i+30);
    context.lineTo(15+30*18+30, 15+30*i+30);
    context.moveTo(15+30*i+30, 15+30);
    context.lineTo(15+30*i+30, 15+30*18+30);
  }
  context.lineWidth = 1; context.strokeStyle = "#666";
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
  context.strokeStyle = "#000";
  context.font = '10px Verdana';
  for(let i=0; i<19; i++) {
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

function oneStep() {
  context.clearRect(0,0,630,630);
  drawChessBoard();
  for (let i = 0; i < 19; i++)
    for (let j = 0; j < 19; j++) {
      if(has_stone[i][j] == flag_white) {
        context.beginPath();
        context.arc(30*i+15+30, 30*j+15+30, 13, 0, Math.PI*2);
        let gradient = context.createRadialGradient(
          30*i+15+30, 30*j+15+30, 13, 30*i+15+30, 30*j+15+30, 0);
        gradient.addColorStop(0, "#b1b1b1");
        gradient.addColorStop(1, "#f9f9f9");
        context.fillStyle = gradient;
        context.fill();
        context.closePath();
      }
      else if (has_stone[i][j] == flag_black) {
        context.beginPath();
        context.arc(30*i+15+30, 30*j+15+30, 13, 0, Math.PI*2);
        let gradient = context.createRadialGradient(
          30*i+15+30, 30*j+15+30, 13, 30*i+15+30, 30*j+15+30, 0);
        gradient.addColorStop(0, "#0a0a0a");
        gradient.addColorStop(1, "#636766");
        context.fillStyle = gradient;
        context.fill();
        context.closePath();
      }
    }
}

FiveChesscanvas.onclick = (e)=>{
  let x = e.offsetX;
  let y = e.offsetY;
  let i = Math.floor(x/30)-1;
  let j = Math.floor(y/30)-1;
  if (i>=0 && j>=0 &&
    has_stone[i][j] == flag_none &&
    is_me) {

    has_stone[i][j] = isWhite()? flag_white: flag_black;
    oneStep();

    let url_ = "/Go/play/b"
    if (isWhite()) {
      url_ = "/Go/play/w";
    }

    url_ += map_a2n[i] + (19 - j).toString();
    console.log("onclick: get: url:" + url_);
    chess_http.open("GET", url_, true);
    chess_http.send();
    is_me = false;
  } else { console.log('invalid click on the chessboard'); }
}

chess_http.onreadystatechange = ()=>{
  if (chess_http.readyState==4 && chess_http.status==200)
  {
    let ret = chess_http.responseText;
    console.log("receive from /Go:" + ret.split("'")[0]);

    if (/[ABCDEFGHJKLMNOPQRST]([1-9]|1\d)/.test(ret)) {

      for (let i = 0; i < 19; i++)
        for (let j = 0; j < 19; j++)
          has_stone[i][j] = flag_none;

      for (let k = 0; k < 19; k++) {
        let k_ = k+1;
        let line = ret.match(RegExp(`\\(${k_} \\(B( \\d{1,2})*\\) \\(W( \\d{1,2})*\\) \\(current( \\d{1,2})*\\)\\)`));
        if (!line) {
          alert("line is null!");
        } else {
          let str = line[0];
          let str_B = str.match(/\(B( \d{1,2})*\)/)[0];
          let str_W = str.match(/\(W( \d{1,2})*\)/)[0];
          let ind_B = str_B.match(/\b\d{1,2}\b/g)
          let ind_W = str_W.match(/\b\d{1,2}\b/g)
          if (ind_B) {
            ind_B.map((str)=>{
              let i = Number(str) - 1;
              let j = 19 - k_;
              has_stone[i][j] = flag_black;
            });
          }
          if (ind_W) {
            ind_W.map((str)=>{
              let i = Number(str) - 1;
              let j = 19 - k_;
              has_stone[i][j] = flag_white;
            });
          }
        }
      }
      oneStep();
      // 恢复点击，继续下棋
      is_me = true;

    } else if (/start ok/.test(ret)) {
      is_me = true;
    } else if (/unfinished play!.*/.test(ret)) {
      alert("引擎已经启动！请重新开始或者继续对决");
    } else if (/stop ok/.test(ret)) {
      is_me = false;
      alert("引擎已关闭")
    } else if (/clear_board ok/.test(ret)) {
      is_me = true;
      clearBoard();
    } else if (/no engine/.test(ret)) {
      alert("没有启动引擎！");
    } else if (/gtp:/.test(ret)) {
      
    } else if (/gtp error/.test(ret)) {
      alert("gtp error!");
    }
  }
};
