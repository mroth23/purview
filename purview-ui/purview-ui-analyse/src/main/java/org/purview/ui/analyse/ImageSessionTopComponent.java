package org.purview.ui.analyse;

import java.awt.BorderLayout;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import org.openide.awt.Mnemonics;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
final class ImageSessionTopComponent extends TopComponent implements ActionListener {

    private static final String ICON_PATH = "org/purview/ui/analyse/gears.png";
    private static final String PREFERRED_ID = "AnalyserSession";
    private final JButton analyseButton;
    private final JScrollPane scrollPane;
    private final JToolBar toolbar;
    private final BufferedImage image;
    private final String name;

    ImageSessionTopComponent(final File imageFile) throws IOException {
        BufferedImage img = ImageIO.read(imageFile);

        BufferedImage argbVersion =
                new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = argbVersion.createGraphics();
        g.drawImage(img, 0, 0, null);
        g.dispose();

        image = argbVersion;
        name = imageFile.getName();

        toolbar = new JToolBar();
        analyseButton = new JButton();
        scrollPane = new CheckerboardScrollPane(new ImagePanel(image));

        setLayout(new BorderLayout());

        toolbar.setRollover(true);

        analyseButton.setIcon(
                new ImageIcon(getClass().getResource("/org/purview/ui/analyse/gears.png")));
        Mnemonics.setLocalizedText(analyseButton,
                NbBundle.getMessage(ImageSessionTopComponent.class, "LBL_AnalyseImage"));
        analyseButton.setFocusable(false);
        analyseButton.setHorizontalTextPosition(SwingConstants.CENTER);
        analyseButton.setVerticalTextPosition(SwingConstants.BOTTOM);
        analyseButton.addActionListener(this);
        toolbar.add(analyseButton);

        add(toolbar, BorderLayout.PAGE_START);
        add(scrollPane, BorderLayout.CENTER);

        setName(NbBundle.getMessage(ImageSessionTopComponent.class,
                "CTL_ImageSessionTopComponent", imageFile.getName()));
        setToolTipText(NbBundle.getMessage(ImageSessionTopComponent.class,
                "HINT_ImageSessionTopComponent"));
        setIcon(ImageUtilities.loadImage(ICON_PATH, true));
    }

    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_NEVER;
    }

    @Override
    protected String preferredID() {
        return PREFERRED_ID;
    }

    public void actionPerformed(ActionEvent e) {
        if(e.getSource() == analyseButton) {
            AnalysisTopComponent c = new AnalysisTopComponent(name, image);
            c.open();
            c.requestActive();

            Thread t = new Thread(c);
            t.setName("Analyser thread for image \"" + name + "\"");
            t.start();
        }
    }
}
