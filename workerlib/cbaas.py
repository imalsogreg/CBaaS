import websocket
import tempfile
from os import remove

def decode_cbaas_argument(msg):
    return 1

class Listener:
    """A work listener for attaching to CBaaS servers"""

    def __init__(self, on_job, host="ws://localhost", port="9160", key=None, verbose=False):

        ws = websocket.WebSocketApp(host,
                                    on_message = handle_message,
                                    on_error   = on_error,
                                    on_close   = on_close)

    def handle_message(ws, message):
        msg_arg = _message_argument(msg)
        v = decode_message_argument(msg)
        return 1


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
    elif k == 'VImage':
        loadThroughTmpImage(v)
    elif k == 'VList':
        return map( _decode_cbaas_value, v )


def _encode_cbaas_value(v):

    t = type(v)
    if (t == type(1) or t == type(1.0)):
        return {'tag':'VDouble', 'contents':v}
    elif (t == type((1+1j))):
        return {'tag':'VPrimComplex',
                'contents':{'real':v.real,'imag':v.imag}}
    elif (t == type('A string')):
        return {'tag':'VText', 'contents':v}
    elif (t == type(numpy.array([[1,2],[3,4]]))):
        return {'tag':'VMat',
                'contents': map( _encode_cbaas_value, v.toList()) }
    else:
        raise Exception('Tried to serialize unknown type: ' + t)

def _loadThroughTmpImage(blob):
    """Load a binary blob into a scikit-image image"""
    mimetype = imghdr.what('', blob);
    if mimetype:
        t  = tempfile.mkstemp(suffix=('.' + mimetype))
        tf = open(t[1],'wb')
        tf.write(blob)
        tf.close()
        i = skimage.io.imread(t[1])
        remove(t[1])
        return i
    else:
        raise Exception('Could not determine image format')


def _message_argument(msg):

    """Extract the argument part from a CBaaS message"""
    try:
        m = msg['contents'][2]
        assert (m['tag']      != None)
        assert (m['contents'] != None)
        return msg['contents'][2]
    except Exception as e:
        raise Exception('Message decoding error, ' + str(e))

