package org.purview.analysers.frontend;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;
import javax.swing.Scrollable;

public final class ImagePanel extends JPanel
        implements Scrollable, MouseMotionListener {

    private final BufferedImage image;

    public ImagePanel(final BufferedImage img) {
        image = img;
        setAutoscrolls(true);
        this.setOpaque(false);
        addMouseMotionListener(this);
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(image, 0, 0, this);
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(image.getWidth(), image.getHeight());
    }

    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    public int getScrollableUnitIncrement(final Rectangle rect, final int orientation, final int direction) {
        return 16;
    }

    public int getScrollableBlockIncrement(final Rectangle rect, final int orientation, final int direction) {
        return 32;
    }

    public boolean getScrollableTracksViewportWidth() {
        return false;
    }

    public boolean getScrollableTracksViewportHeight() {
        return false;
    }

    public void mouseDragged(final MouseEvent me) {
        final Rectangle r = new Rectangle(me.getX(), me.getY(), 1, 1);
        this.scrollRectToVisible(r);
    }

    public void mouseMoved(final MouseEvent me) {
    }
}
