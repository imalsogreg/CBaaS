function r = var getStimulusById(id)
  opts = weboptions();

  r = webread('/stimulus/' + encodeURIComponent(id) + '', ...
    TODO...
    , opts);

end

function r = var postStimulus(body)
  opts = weboptions();

  r = webwrite('/stimulus', ...
    TODO, ...
    JSON.stringify(body)
...
    , opts);

function r = var putStimulusById(id, body)
  u = java.net.URL('/stimulus/' + encodeURIComponent(id) + '');
  conn = u.openConnection();
  in = java.io.BufferedReader ...
       (java.io.InputStreamReader (conn.getInputStream ()));
  doneReading = false;
  while (~doneReading)
    newStr = in.readLine();
    str    = [str, newStr]
    if (isempty(str))
      doneReading = true;
    end
  end
  r = parse_json(str)
end

function r = var deleteStimulusById(id)
  u = java.net.URL('/stimulus/' + encodeURIComponent(id) + '');
  conn = u.openConnection();
  in = java.io.BufferedReader ...
       (java.io.InputStreamReader (conn.getInputStream ()));
  doneReading = false;
  while (~doneReading)
    newStr = in.readLine();
    str    = [str, newStr]
    if (isempty(str))
      doneReading = true;
    end
  end
  r = parse_json(str)
end

function r = var getFeatureById(id)
  opts = weboptions();

  r = webread('/feature/' + encodeURIComponent(id) + '', ...
    TODO...
    , opts);

end

function r = var postFeature(body)
  opts = weboptions();

  r = webwrite('/feature', ...
    TODO, ...
    JSON.stringify(body)
...
    , opts);

function r = var putFeatureById(id, body)
  u = java.net.URL('/feature/' + encodeURIComponent(id) + '');
  conn = u.openConnection();
  in = java.io.BufferedReader ...
       (java.io.InputStreamReader (conn.getInputStream ()));
  doneReading = false;
  while (~doneReading)
    newStr = in.readLine();
    str    = [str, newStr]
    if (isempty(str))
      doneReading = true;
    end
  end
  r = parse_json(str)
end

function r = var deleteFeatureById(id)
  u = java.net.URL('/feature/' + encodeURIComponent(id) + '');
  conn = u.openConnection();
  in = java.io.BufferedReader ...
       (java.io.InputStreamReader (conn.getInputStream ()));
  doneReading = false;
  while (~doneReading)
    newStr = in.readLine();
    str    = [str, newStr]
    if (isempty(str))
      doneReading = true;
    end
  end
  r = parse_json(str)
end

function r = var postUserLogin(body)
  opts = weboptions();

  r = webwrite('/user/login', ...
    TODO, ...
    JSON.stringify(body)
...
    , opts);

function r = var postUserRegister(body)
  opts = weboptions();

  r = webwrite('/user/register', ...
    TODO, ...
    JSON.stringify(body)
...
    , opts);

function r = var getUserUser()
  opts = weboptions();

  r = webread('/user/user', ...
    TODO...
    , opts);

end

function r = var postUserLogout()
  opts = weboptions();

  r = webwrite('/user/logout', ...
    TODO, ...
    null...
    , opts);

