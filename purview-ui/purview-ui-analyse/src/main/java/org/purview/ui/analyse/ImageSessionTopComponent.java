package org.purview.ui.analyse;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
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
import org.openide.windows.WindowManager;
import org.purview.analysers.DefaultAnalysers;
import org.purview.core.analysis.Analyser;
import org.purview.core.data.Color;
import org.purview.core.data.Matrix;
import scala.Function0;
import scala.collection.Iterator;
import scala.collection.Seq;

/**
 * Top component which displays something.
 */
final class ImageSessionTopComponent extends TopComponent implements ActionListener {

    private static final String ICON_PATH = "org/purview/ui/analyse/gears.png";
    private static final String PREFERRED_ID = "ImageSessionTopComponent";
    private final JButton analyseButton;
    private final JButton configureAnalysersButton;
    private final JScrollPane scrollPane;
    private final JToolBar toolbar;
    private final BufferedImage image;
    private final String name;
    private final Map<Analyser<Matrix<Color>>, Boolean> analysers;

    ImageSessionTopComponent(final File imageFile) throws IOException {
        //Load the image
        BufferedImage img = ImageIO.read(imageFile);

        //Convert image to the internal ARGB format
        BufferedImage argbVersion =
                new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = argbVersion.createGraphics();
        g.drawImage(img, 0, 0, null);
        g.dispose();

        //Store the image
        image = argbVersion;
        name = imageFile.getName();

        //Do some really nifty Scala interop conversions to load default analysers
        Seq<Function0<Analyser<Matrix<Color>>>> analyserFactories = DefaultAnalysers.analysers();
        Iterator<Function0<Analyser<Matrix<Color>>>> iter = analyserFactories.iterator();
        HashMap<Analyser<Matrix<Color>>, Boolean> createdAnalysers =
                new HashMap<Analyser<Matrix<Color>>, Boolean>();

        while (iter.hasNext()) {
            Function0<Analyser<Matrix<Color>>> c = iter.next();
            createdAnalysers.put(c.apply(), Boolean.TRUE);
        }
        analysers = createdAnalysers;

        //Create GUI
        toolbar = new JToolBar();
        analyseButton = new JButton();
        configureAnalysersButton = new JButton();
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

        configureAnalysersButton.setIcon(
                new ImageIcon(getClass().getResource("/org/purview/ui/analyse/settings.png")));
        Mnemonics.setLocalizedText(configureAnalysersButton,
                NbBundle.getMessage(ImageSessionTopComponent.class, "LBL_ConfigureAnalysers"));
        configureAnalysersButton.setFocusable(false);
        configureAnalysersButton.setHorizontalTextPosition(SwingConstants.CENTER);
        configureAnalysersButton.setVerticalTextPosition(SwingConstants.BOTTOM);
        configureAnalysersButton.addActionListener(this);
        toolbar.add(configureAnalysersButton);

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
        if (e.getSource() == analyseButton) {
            LinkedList<Analyser<Matrix<Color>>> activeAnalysers = new LinkedList<Analyser<Matrix<Color>>>();
            for (Analyser<Matrix<Color>> key : analysers.keySet()) {
                if (analysers.get(key)) {
                    activeAnalysers.add(key);
                }
            }
            AnalysisTopComponent c = new AnalysisTopComponent(name, image, activeAnalysers);
            c.open();
            c.requestActive();

            Thread t = new Thread(c);
            t.setName("Analyser thread for image \"" + name + "\"");
            t.start();
        } else if (e.getSource() == configureAnalysersButton) {
            Frame window = WindowManager.getDefault().getMainWindow();
            AnalyserSettingsDialog dialog = new AnalyserSettingsDialog(analysers, window, true);
            dialog.setVisible(true);
        }
    }
}
