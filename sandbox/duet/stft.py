#!/usr/bin/env python
# -*- coding:utf-8 -*-
import scipy
import pylab

def stft(x, framesample, stepsample, window=[] ):

    if window == []:
        window = scipy.hamming(framesample)
    X = scipy.array([scipy.fft( window * x[ i:i+framesample ] )
        for i in range( 0, len(x)-framesample, stepsample ) ] )
    return X

def istft(X, stepsample, iwindow=[], window=[] ):
    x = scipy.zeros( calc_signal_length( X, stepsample ) )
    framesample = X.shape[1]

    window = gen_sync_window( iwindow, window, framesample, stepsample )

    for n,i in enumerate(range(0, len(x)-framesample, stepsample)):
        x[i:i+framesample] +=  window * scipy.real(scipy.ifft(X[n]))
    return x

def calc_signal_length( X, stepsample ):
    freq_num, frame_num = X.shape
    return freq_num + frame_num * stepsample

def gen_sync_window( iwindow, window, framesample, stepsample ):
    if iwindow == []:
        iw = 1. / scipy.hamming( framesample )
    else:
        iw = iwindow

    if window == []:
        w = scipy.zeros( framesample )
        beg = ( framesample - 2 * stepsample ) / 2
        end = beg + 2 * stepsample
        w[ beg:end ] = scipy.hamming( 2 * stepsample )
    else:
        w = window
    return iw * w

def main():
    f0 = 440         # Compute the STFT of a 440 Hz sinusoid
    fs = 8000        # sampled at 8 kHz
    T = 5            # lasting 5 seconds
    framesz = 0.050  # with a frame size of 50 milliseconds
    hop = 0.004      # and hop size of 20 milliseconds.

    # Create test signal and STFT.
    t = scipy.linspace(0, T, T*fs, endpoint=False)
    x = scipy.sin(2*scipy.pi*f0*t)
    X = stft(x, int(fs * framesz), int(fs * hop) )

    # Plot the magnitude spectrogram.
    pylab.figure()
    pylab.imshow(scipy.absolute(X.T), origin='lower', aspect='auto',
    interpolation='nearest')
    pylab.xlabel('Time')
    pylab.ylabel('Frequency')
    pylab.show()

    # Compute the ISTFT
    xhat = istft(X, int( fs * hop ) )

    # Plot the input and output signals over 0.1 seconds.
    T1 = int(0.1*fs)

    pylab.figure()
    pylab.plot(t[:T1], x[:T1], t[:T1], xhat[:T1])
    pylab.xlabel('Time (seconds)')

    pylab.figure()
    pylab.plot(t[-T1:], x[-T1:], t[-T1:], xhat[-T1:])
    pylab.xlabel('Time (seconds)')
    pylab.show()

    return

if __name__ == '__main__':
    main()
