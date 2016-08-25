import base64
import numpy
import requests as requests
import websocket
import logging
import imghdr
import skimage
import skimage.io
import tempfile
from os import remove
from json import dumps, loads
import logging
import thread
import time

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

    def __init__(self, on_job, domain="localhost", port="9160", key=None, verbose=False, restart=True):
        self._workerID = None
        self._wsHost     = "ws://" + domain
        self._httpHost   = "http://" + domain
        self._on_job   = on_job
        _wsUrl = self._wsHost + '/api1/work?name=test&function=fix'
        ws = websocket.WebSocketApp(self._wsHost + '/api1/work?name=test&function=fix',
                                    on_close   = lambda msg: show_close(msg),
                                    on_message = self._handle_message,
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

    def _handle_message(self, ws, msgstr):
        print ("(cbaas) HANDLE_MESSAGE")

        msg = loads(msgstr)

        print 'Message is loaded'
        if (_isJob(msg)):

            print 'Is Job'
            print 'ID: ' + str(msg['contents'][0])
            if self._workerID == None:
                raise Exception("Job requested without _workerID defined")

            respUrl = self._httpHost + '/api1/returnfun'
            respParams = { 'worker-id': self._workerID,
                           'job-id':    msg['contents'][0]}
            respHeaders = {'Content-Type': 'application/json'}

            msg_arg = _message_argument(msg)

            v = _decode_cbaas_value(msg_arg)

            r = self._on_job(v)
            print "r: " + str(r)

            cbaas_r = _encode_cbaas_value(r)
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


def _encode_cbaas_value(v):
    t = type(v)
    # TODO: find isReal(type)
    if (t == type(1) or t == type(1.0) or t == type(numpy.int64(1))):
        return {'tag':'VDouble', 'contents':v}
    elif (t == type((1+1j))):
        return {'tag':'VPrimComplex',
                'contents':{'real':v.real,'imag':v.imag}}
    elif (t == type('A string') or t == type(u'A unicode string')):
        return {'tag':'VText', 'contents':v}
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
