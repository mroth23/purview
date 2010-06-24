package org.purview.core.transforms;

import edu.emory.mathcs.jtransforms.dct.FloatDCT_1D;
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D;

public class JTransforms1D {

    private FloatDCT_1D dct1d;
    private FloatFFT_1D fft1d;

    public JTransforms1D(int length) {
        dct1d = new FloatDCT_1D(length);
        fft1d = new FloatFFT_1D(length);
    }

    public float[] DCT1DForward(float[] input, boolean scale) {
        float[] data = input.clone();
        dct1d.forward(data, scale);
        return data;
    }

    public float[] DCT1DInverse(float[] input, boolean scale) {
        float[] data = input.clone();
        dct1d.inverse(data, scale);
        return data;
    }

    public float[] FFT1DForward(float[] input) {
        float[] data = input.clone();
        fft1d.complexForward(data);
        return data;
    }

    public float[] FFT1DInverse(float[] input, boolean scale) {
        float[] data = input.clone();
        fft1d.complexInverse(data, scale);
        return data;
    }
}
