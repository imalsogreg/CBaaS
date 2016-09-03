import base64
import numpy
import requests as requests
import websocket
import logging
import imghdr
import PIL.Image
import skimage
import skimage.io as io
import skimage.io
from StringIO import StringIO
import tempfile
from os import remove
from json import dumps, loads
import logging
import thread
import time
import urllib

io.use_plugin('pil')

# namespaces (numpy as np)
# context managers
# own Exception class
# don't catch all exceptions
# beware of lambdas

logging.basicConfig()

class DecodeError(Exception):
    def __init__(self, msg):
        self.msg = msg

class Listener:
    """A work listener for attaching to CBaaS servers"""

    def __init__(self, on_job, function_name, type, domain="localhost", port="9160", worker_name="anonymous", key=None, verbose=False, restart=True):
        self._workerID   = None
        self._wsHost     = "wss://" + domain
        self._httpHost   = "https://" + domain
        self._on_job     = on_job
        self._type       = type
        # _wsUrl = self._wsHost + '/api1/work?name=test&function=fix'
        _wsUrl = self._wsHost + '/api1/work?' + urllib.urlencode({'type': self._type, 'function': function_name, 'name': worker_name})
        ws = websocket.WebSocketApp(_wsUrl, # self._wsHost + '/api1/work?name=test&function=fix',
                                    on_close   = lambda msg: show_close(msg),
                                    on_message = lambda ws, msg: self._handle_message(msg, type),
                                    on_error   = show_err,
                                    on_open    = hold_open,
                                    on_ping    = lambda ws, payload: _handle_ping(ws, payload),
                                    on_pong    = lambda ws, payload: _handle_pong(ws, payload)
                                   )

        print "Init about to run_forever"
        ws.run_forever()

        while(restart):
          print "Disconnected. Reconnect in 5 seconds..."
          time.sleep(5)
          ws.run_forever()

    def _handle_message(self, msgstr, ty):
        print ("(cbaas) HANDLE_MESSAGE")

        msg = loads(msgstr)

        print 'Message is loaded'
        if (_isJob(msg)):

            print 'Is Job'
            print 'ID: ' + str(msg['contents'][0])
            if self._workerID == None:
                raise Exception("Job requested without _workerID defined")

            respUrl = self._httpHost + '/api1/returnfun'
            print 'respUrl: ' + respUrl

            print 'msg:'
            # print msg['contents'][0]
            respParams = { 'worker-id': self._workerID,
                           'job-id':    msg['contents']}
            print 'respParams: '
            # print respParams

            respHeaders = {'Content-Type': 'application/json'}

            msg_arg = _message_argument(msg)
            print 'msg_arg: '
            # print msg_arg

            v = _decode_cbaas_value(msg_arg)
            print '_decode_cbaas_value: '
            print v

            r = self._on_job(v)
            print "r: "
            print r

            cbaas_r = _encode_cbaas_value(r, retTypeOfType(ty))
            msg_r = {'tag':'WorkerFinished',
                     'contents':[
                         msg['contents'][0],
                         {'job':msg['contents'][0],'value':cbaas_r}
                     ]}
            job_result = {'job': msg['contents'][0], 'value': cbaas_r }
            # ws.send(dumps(msg_r))
            print "PRE"
            print "cbaas_r :"
            print cbaas_r
            print "job_result: "
            print job_result
            resp = requests.post(respUrl, params=respParams,
                                 json=job_result, headers=respHeaders)

            print resp
            print resp.text
            print "POST"

        else:
            try:
                assert msg['tag'] != None
                if msg['tag'] == 'WorkerSetID':
                    print "Set worker id to " + msg['contents']
                    assert msg['contents'] != None
                    self._workerID = msg['contents']
            except Exception as e:
                raise Exception('SetID: Message decoding error, ' + str(e))

def msg_job(msg):
    j = msg['contents'][1]
    try:
        assert j['function'] != None
        assert j['arg']      != None
    except Exception as e:
        raise Exception('message contains faulty job: ' + j)

def _isJob(msg):
    try:
        assert msg['tag'] != None
        return msg['tag'] == 'JobRequested'
    except Exception as e:
        raise Exception('Messagetype: Message decoding error, ' + str(e))



def _message_argument(msg):

    """Extract the argument part from a CBaaS message"""
    try:
        m = msg['contents'][1]
        assert (m['arg']['tag']      != None)
        assert (m['arg']['contents'] != None)
        return m['arg']

    except Exception as e:
        raise Exception('Argument: Message decoding error, ' + str(e))

def _handle_ping(ws, payload):
    ws.sock.pong(payload)

def _handle_pong(ws,payload):
    print "CALLBACK PONG"

def _decode_cbaas_value(kv):
    """Convert a CBaaS JSON-encoded value into a Python value
       The CBaaS values are listed here:
       https://github.com/CBMM/CBaaS/blob/master/src/Model.hs
    """

    k = kv['tag']
    v = kv['contents']

    if k == 'VDouble':
        return v
    elif k == 'VPrimComplex':
        return v['real'] + (0+1j) * v['imag']
    elif k == 'VText':
        return v
    elif k == 'VList':
        return map( _decode_cbaas_value, v )
    elif k == 'VImage':
        if v['tag'] == 'ModelImage':
            bytestring = base64.b64decode( v['contents'] )
            img = _load_through_tmp_image(bytestring)
            return img
        else:
            raise ValueError('Improperly formatted VImage', kv)


def _encode_cbaas_value(v,ty):
    t = type(v)
    print "TYPE: "
    print ty
    # TODO: find isReal(type)
    if (t == type(1) or t == type(1.0) or t == type(numpy.int64(1))):
        return {'tag':'VDouble', 'contents':v}
    elif (t == type((1+1j))):
        return {'tag':'VPrimComplex',
                'contents':{'real':v.real,'imag':v.imag}}
    elif (t == type('A string') or t == type(u'A unicode string')):
        return {'tag':'VText', 'contents':v}
    elif (ty == 'TModelImage'):
        s = StringIO()
        fakev = numpy.array([[100.0,200.0],[0.0,1.0]])
        skimage.io.imsave(s, arr=numpy.uint8(fakev), plugin='pil')
        imgdat = base64.b64encode(s.getvalue())

        mi = {'tag': 'ModelImage',
              'contents': imgdat}
        v = {'tag':'VImage', 'contents': (mi) }
        print v
        return v
    elif (t == type(numpy.array([[1,2],[3,4]]))):
        return {'tag':'VMat',
                'contents': map( _encode_cbaas_value, v.toList()) }
    else:
        print 'Tried to serialize unknown type'
        print t
        raise Exception('Tried to serialize unknown type')

def show_open(message):
  print 'CBaaS websocket OPEN (message)'
  print message


def hold_open(ws):
  def run (*args):
    while True:
      ws.send("worker ping")
      time.sleep(1)
  thread.start_new_thread(run, ())


def show_err(ws, e):
  print ('CBaaS websocket ERROR')
  print e

def show_close(ws):
  print ('CBaaS websocket CLOSE')

def package_image_binary(binary_blob):
    """Create a temporary file for an image and return its filename.
       The file will be created in your system's temp directory,
       but you should probably delete it after use anyway.
    """
    mimetype = imghdr.what('',binary_blob)
    print "NEW VERSION2"
    if mimetype == None:
        raise Exception('Failed to identify image format for binary.')
    else:
        t  = tempfile.mkstemp(suffix=('.' + mimetype))
        with open(t[1],'wb') as tf1:
            tf1.write(binary_blob)
        print t[1]
        return t[1]



def _load_through_tmp_image(blob):
    """Load a binary blob into a scikit-image image"""
    mimetype = imghdr.what('', blob);
    if mimetype:
        t  = tempfile.mkstemp(suffix=('.' + mimetype))
        with open(t[1],'wb') as tf:
            tf.write(blob)
        i = skimage.img_as_float(skimage.io.imread(t[1]))
        remove(t[1])
        return i
    else:
        raise Exception('Could not determine image format')

# This main function is only meant for testing this library.
# Not meant for external use.
if __name__ == "__main__":
  def work(x):
    print "Working on: "
    print x
    return numpy.prod(x.shape)
  print "Main!"
  l = Listener(on_job=work, domain="localhost:8000", verbose=True)
  print "Finished"

# TODO: Fix. This will break on (Int -> Int) -> Int
def retTypeOfType(ty):
    return ty.split('->',1)[1].strip()
