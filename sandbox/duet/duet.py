#!/usr/bin/env python
# -*- coding:utf-8 -*-
from __future__ import division
import scipy as sp
from scipy import io
from scipy.io import wavfile
import pylab as pyl
import urllib2
import wave
import sys
import re

def stft(xs, framesample, stepsample, window=[] ):
    channel,signal_length = xs.shape
    frames = ( signal_length - framesample ) // stepsample + 1
    freqs = framesample / 2 + 1
    Xs = sp.zeros( ( channel, frames, freqs ),complex )

    for i in xrange( 0, channel ):
        x = xs[i,:]
        if window == []:
            window = sp.hamming(framesample)
        Xs[i,:,:] = sp.array([ convert_real_signal_spectol( sp.fft( window * x[ j:j+framesample ] ) )
            for j in range( 0, len(x)-framesample, stepsample ) ] )
    return Xs

def istft(Xs, stepsample, iwindow=[], window=[] ):
    channels,frame_num,freq_num = Xs.shape
    framesample = freq_num
    window = gen_sync_window( iwindow, window, framesample, stepsample )

    xs = sp.zeros( ( channels, calc_signal_length( freq_num, frame_num, stepsample ) ) )
    for i in xrange( 0, channels ):
        x = xs[i,:]
        X = Xs[i]
        for n,j in enumerate(range(0, len(x)-framesample, stepsample)):
            x[j:j+framesample] +=  window * sp.real(sp.ifft(X[n]))
    return xs

def calc_signal_length( freq_num, frame_num, stepsample ):
    return freq_num + frame_num * stepsample

def gen_sync_window( iwindow, window, framesample, stepsample ):
    if iwindow == []:
        iw = 1. / sp.hamming( framesample )
    else:
        iw = iwindow

    if window == []:
        w = sp.zeros( framesample )
        beg = ( framesample - 2 * stepsample ) / 2
        end = beg + 2 * stepsample
        w[ beg:end ] = sp.hamming( 2 * stepsample )
    else:
        w = window
    return iw * w

def read_wav( uri ):
    url_pattern = re.compile('http://.*')
    if url_pattern.match( uri ):
        rate,data = read_wav_from_web( uri )
    else:
        rate, data = sp.io.wavfile.read( uri )

    return rate,data.T

def read_wav_from_web( url ):
    wi = wave.open( urllib2.urlopen( url ).fp, 'rb' )
    data = sp.fromstring( wi.readframes( wi.getnframes() ), sp.int16 )
    data = sp.reshape( data, ( data.shape[0] / wi.getnchannels(), wi.getnchannels() ) )
    rate = wi.getframerate()

    return rate,data

def sat_int16( sample ):
    res = sample
    if 32767 < res:
        res = 32767
    elif res < -32768:
        res = -32768

    return res

def convert_int16( sig ):
    return sp.array( map( lambda x: sp.int16( sat_int16( x ) ) , sig ) )

def convert_real_signal_spectol( spectol ):
    valid_freqs =  spectol.shape[0] / 2 + 1
    return spectol[0:valid_freqs]

def main():
    try:
        uri = sys.argv[1]
    except:
        uri = 'http://www.ism.ac.jp/~shiro/research/sounds/RSS/X_rss.wav'

    rate,data = read_wav( uri )
    X = stft( data, 1024, 512 )

    X0X1_ratio = ( X[0] + 0.00000001) /  ( X[1] + 0.00000001 )
    amp_ratio = sp.absolute( X0X1_ratio )
    alpha = amp_ratio - ( 1.0 / amp_ratio )

    phase_diff = sp.log( X0X1_ratio ).imag
    fmat = [ sp.linspace( 1, sp.pi, phase_diff.shape[1] )  for i in xrange(0, phase_diff.shape[0] ) ]
    delta = -phase_diff / fmat

    pyl.figure()
    valid_samples =  sp.logical_and(( sp.absolute( alpha ) < 0.7 ) , ( sp.absolute( delta ) < 3.6 ))
    #pyl.imshow( sp.log( sp.absolute( alpha ) ) )
    #pyl.imshow( sp.absolute( delta )  )
    pyl.scatter( alpha[ valid_samples ], delta[ valid_samples ] )
    pyl.show()

    xhat = istft(X, 256 )
    #pyl.figure()
    #pyl.plot( xhat[0,:] )
    #pyl.show()

    sp.io.wavfile.write( 'xhat_r.wav', rate, convert_int16(xhat[0,:]) )
    sp.io.wavfile.write( 'xhat_l.wav', rate, convert_int16(xhat[1,:]) )
    #sp.io.wavfile.write( 'out_L.wav', rate, data[:,1] )

    return

if __name__ == '__main__':
    main()
