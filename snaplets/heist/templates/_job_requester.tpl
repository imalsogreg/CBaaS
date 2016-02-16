<div class="job-requester">

  <h2>Call function</h2>

  <form>
    <input type="text"
           placeholder="reverse" id="fun-name"/>
    <input type="text"
           placeholder="Hello" id="fun-args"/>
    <button class="btn btn-default"
            type="button" id="call-button"
            onclick="callFun()">Call</button>
  </form>

  <br/>

  <h2>Evaluate expression</h2>
  <div class="input-group">
    <input type="text" class="form-control"
           placeholder="Expression..." id="expr-text"/>
    <span class="input-group-btn">
      <button class="btn btn-default" type="button" id="go">Go!</button>
    </span>
  </div>

  <div class="workers">
    <table id="workers">
      
    </table>
  </div>

  <h2>Picture</h2>
  <div class="pic-requests">
    <input type="text" class="form-control"
           placeholder="pixels" id="pic-fun-name"/>
    <input type="text" class="form-control" id="pic-fun-arg"
           onkeyup="updatePicLoaded()"
           placeholder="img.jpg"/>
    <button class="btn btn-default" type="button" id="pic-go"
            onclick="callPicFun()">
      Send Pic
    </button>
  </div>

  <h4>Img</h4>
  <img id="tmpimg"/>

  <h4>Canvas</h4>
  <canvas id="imgcanvas"></canvas>

  <script src="js/job_requester.js"></script>
  <script src="js/list_workers.js"></script>

</div>