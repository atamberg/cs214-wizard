var state = "bidding";

window.addEventListener("load", () => {
  document.querySelector("#" + state).style.display = "grid";
  document.querySelector("#current-state").innerText = state;
});

function changeState(to) {
  document.querySelector("#" + state).style.display = "none";
  state = to;
  document.querySelector("#" + state).style.display = "grid";
  document.querySelector("#current-state").innerText = state;
}