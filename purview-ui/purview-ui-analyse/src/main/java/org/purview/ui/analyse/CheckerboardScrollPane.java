package org.purview.ui.analyse;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.JScrollPane;

public class CheckerboardScrollPane extends JScrollPane {

    private final int SQUARE_SIZE = 16;

    public CheckerboardScrollPane() {
        viewport.setOpaque(false);
    }

    public CheckerboardScrollPane(Component c) {
        super(c);
        viewport.setOpaque(false);
    }

    @Override
    public void paintComponent(Graphics g) {
        if (!isOpaque()) {
            super.paintComponent(g);
            return;
        }

        g.setColor(Color.gray);
        g.fillRect(0, 0, getWidth(), getHeight());

        g.setColor(Color.lightGray);
        for (int x = 0; x <= getWidth() / SQUARE_SIZE; x++) {
            for (int y = 0; y <= getHeight() / SQUARE_SIZE; y++) {
                if ((x + y) % 2 == 1) {
                    g.fillRect(x * SQUARE_SIZE, y * SQUARE_SIZE, SQUARE_SIZE, SQUARE_SIZE);
                }
            }
        }

        setOpaque(false);
        super.paintComponent(g);
        setOpaque(true);
    }
}
