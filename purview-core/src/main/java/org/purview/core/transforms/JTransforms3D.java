package org.purview.core.transforms;

import edu.emory.mathcs.jtransforms.dct.FloatDCT_3D;
import edu.emory.mathcs.jtransforms.fft.FloatFFT_3D;

public class JTransforms3D {

    private FloatDCT_3D dct3d;
    private FloatFFT_3D fft3d;

    public JTransforms3D(int x, int y, int z) {
        dct3d = new FloatDCT_3D(x, y, z);
        fft3d = new FloatFFT_3D(x, y, z);
    }

    public float[][][] DCT3DForward(float[][][] input, boolean scale) {
        float[][][] data = input.clone();
        dct3d.forward(data, scale);
        return data;
    }

    public float[][][] DCT3DInverse(float[][][] input, boolean scale) {
        float[][][] data = input.clone();
        dct3d.inverse(data, scale);
        return data;
    }

    public float[][][] FFT3DForward(float[][][] input) {
        float[][][] data = input.clone();
        fft3d.complexForward(data);
        return data;
    }

    public float[][][] FFT3DInverse(float[][][] input, boolean scale) {
        float[][][] data = input.clone();
        fft3d.complexInverse(data, scale);
        return data;
    }
}
