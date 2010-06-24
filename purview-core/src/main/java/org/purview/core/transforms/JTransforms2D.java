package org.purview.core.transforms;

import edu.emory.mathcs.jtransforms.dct.FloatDCT_2D;
import edu.emory.mathcs.jtransforms.fft.FloatFFT_2D;

public class JTransforms2D {

    private FloatDCT_2D dct2d;
    private FloatFFT_2D fft2d;

    public JTransforms2D(int width, int height) {
        dct2d = new FloatDCT_2D(width, height);
        fft2d = new FloatFFT_2D(width, height);
    }

    public float[][] DCT2DForward(float[][] input, boolean scale) {
        float[][] data = input.clone();
        dct2d.forward(data, scale);
        return data;
    }

    public float[][] DCT2DInverse(float[][] input, boolean scale) {
        float[][] data = input.clone();
        dct2d.inverse(data, scale);
        return data;
    }

    public float[][] FFT2DForward(float[][] input) {
        float[][] data = input.clone();
        fft2d.complexForward(data);
        return data;
    }

    public float[][] FFT2DInverse(float[][] input, boolean scale) {
        float[][] data = input.clone();
        fft2d.complexInverse(data, scale);
        return data;
    }
}
