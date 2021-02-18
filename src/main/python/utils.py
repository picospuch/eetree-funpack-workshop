import SoapySDR
from SoapySDR import SOAPY_SDR_CRITICAL


def parseSaveStr(memory, name):
    return str(memory[name]["freq"]/1e6) + " " + memory[name]["band"]


def isCudaCapable():
    try:
        import cupy
        import cusignal
    except ImportError:
        return False
    return True


def defaultFavorites():
    return {
            "memA": {
                "freq": 102.3e6,
                "band": "FM"
            },
            "memB": {
                "freq": 103.2e6,
                "band": "FM"
            },
            "memC": {
                "freq": 88.1e6,
                "band": "FM"
            },
            "memD": {
                "freq": 93.6e6,
                "band": "FM"
            },
            "memE": {
                "freq": 104.1e6,
                "band": "FM"
            },
        }
