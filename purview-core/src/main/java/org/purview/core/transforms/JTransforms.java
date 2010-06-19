package org.purview.core.transforms;

import edu.emory.mathcs.jtransforms.dct.FloatDCT_1D;
import edu.emory.mathcs.jtransforms.dct.FloatDCT_2D;

class JTransforms {

    public JTransforms() {
    }

    public float[] DCT1DForward(float[] input, boolean scale) {
        float[] data = input.clone();
        FloatDCT_1D dct1d = new FloatDCT_1D(data.length);
        dct1d.forward(data, scale);
        return data;
    }

    public float[] DCT1DInverse(float[] input, boolean scale) {
        float[] data = input.clone();
        FloatDCT_1D dct1d = new FloatDCT_1D(data.length);
        dct1d.inverse(data, scale);
        return data;
    }

    public float[][] DCT2DForward(float[][] input, boolean scale) {
        float[][] data = input.clone();
        FloatDCT_2D dct2d = new FloatDCT_2D(data.length, data[0].length);
        dct2d.forward(data, scale);
        return data;
    }

    public float[][] DCT2DInverse(float[][] input, boolean scale) {
        float[][] data = input.clone();
        FloatDCT_2D dct2d = new FloatDCT_2D(data.length, data[0].length);
        dct2d.inverse(data, scale);
        return data;
    }
}
