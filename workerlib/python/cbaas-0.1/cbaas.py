import base64
import numpy
import websocket
import logging
import imghdr
import skimage
import skimage.io
import tempfile
from os import remove
from json import dumps, loads
import logging
import time

logging.basicConfig() # TODO is this the right time to run this?

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
        tf1 = open(t[1],'wb')
        tf1.write(binary_blob)
        tf1.close()
        print t[1]
        return t[1]

class Listener:
    """A work listener for attaching to CBaaS servers"""

    def __init__(self, on_job, host="ws://localhost", port="9160", key=None, verbose=False, restart=True):

        ws = websocket.WebSocketApp(host + '/api1/work?name=test&function=fix',
                                    on_close   = lambda msg: show_close(msg),
                                    on_message = lambda ws, msg: _handle_message(ws,msg,on_job), 
                                    on_error   = show_err,
                                    on_open    = show_open,
                                    on_ping    = lambda ws, payload: _handle_ping(ws, payload),
                                    on_pong    = lambda ws, payload: _handle_pong(ws, payload)
                                   )

        print "Init about to run_forever"
        ws.run_forever()
        
        while(restart):
          print "Disconnected. Reconnect in 5 seconds..."
          time.sleep(5)
          ws.run_forever()
        print "Init finished run_forever"

def _handle_ping(ws, payload):
    print "CALLBACK PING, sending PONG"
    print "PING PAYLOAD:"
    print payload
    ws.sock.pong(payload)
    print "Send pong"

def _handle_pong(ws,payload):
    print "CALLBACK PONG"

def _handle_message(ws, msgstr, on_job):
    print ("(cbaas) HANDLE_MESSAGE")

    msg = loads(msgstr)

    msg_arg = _message_argument(msg)

    v = _decode_cbaas_value(msg_arg)

    r = on_job(v)

    cbaas_r = _encode_cbaas_value(r)
    msg_r = {'tag':'WorkerFinished',
             'contents':[
               msg['contents'][0],
               msg['contents'][1],
               {'job':msg['contents'][0],'value':cbaas_r}
             ]
            }
    ws.send(dumps(msg_r))

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

def show_err(ws, e):
  print ('CBaaS websocket ERROR')
  print e

def show_close(ws):
  print ('CBaaS websocket CLOSE')


def _load_through_tmp_image(blob):
    """Load a binary blob into a scikit-image image"""
    mimetype = imghdr.what('', blob);
    if mimetype:
        t  = tempfile.mkstemp(suffix=('.' + mimetype))
        tf = open(t[1],'wb')
        tf.write(blob)
        tf.close()
        i = skimage.img_as_float(skimage.io.imread(t[1]))
        remove(t[1])
        return i
    else:
        raise Exception('Could not determine image format')


def _message_argument(msg):

    """Extract the argument part from a CBaaS message"""
    try:
        m = msg['contents'][2]
        assert (m['arg']['tag']      != None)
        assert (m['arg']['contents'] != None)
        return m['arg']

    except Exception as e:
        raise Exception('Message decoding error, ' + str(e))

if __name__ == "__main__":
  def work(x):
    print "Working on: "
    print x
    return numpy.prod(x.shape)
    # return x[::-1]
  print "Main!"
  l = Listener(on_job=work, host="ws://localhost", verbose=True)
  print "Finished"


