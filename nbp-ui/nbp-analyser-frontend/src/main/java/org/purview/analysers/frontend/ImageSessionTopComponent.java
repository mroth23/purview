package org.purview.analysers.frontend;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
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
import org.purview.core.analysis.Analyser;
import org.purview.core.data.ImageMatrix;
import org.purview.core.session.SessionUtils;
import scala.collection.Iterator;
import scala.collection.Seq;

/**
 * Top component which displays an image session.
 */
final class ImageSessionTopComponent extends TopComponent implements ActionListener {
    //Some needed resources

    private static final String ICON_PATH = "org/purview/analysers/frontend/gears.png";
    private static final String PREFERRED_ID = "ImageSessionTopComponent";
    private static final String analyseButtonText = NbBundle.getMessage(ImageSessionTopComponent.class, "LBL_AnalyseImage");
    private static final Image componentIcon = ImageUtilities.loadImage(ICON_PATH, true);
    private static final String componentTooltip = NbBundle.getMessage(ImageSessionTopComponent.class, "HINT_ImageSessionTopComponent");
    private static final String configureAnalysersButtonText = NbBundle.getMessage(ImageSessionTopComponent.class, "LBL_ConfigureAnalysers");
    //Some needed widgets
    private final JButton analyseButton;
    private final JButton configureAnalysersButton;
    private final JScrollPane scrollPane;
    private final JToolBar toolbar;
    private final ImageMatrix image;
    private final String name;
    //Some needed analyser-related variables
    private final Map<Analyser<ImageMatrix>, Boolean> analysers;

    ImageSessionTopComponent(final File imageFile) throws IOException {
        //Store the image
        image = ImageMatrix.fromFile(imageFile);
        name = imageFile.getName();

        //Do some really nifty Scala interop conversions to load the default analysers
        Seq<Analyser<ImageMatrix>> analyserSequence = SessionUtils.createAnalyserInstances();
        Iterator<Analyser<ImageMatrix>> iter = analyserSequence.iterator();

        analysers = new HashMap<Analyser<ImageMatrix>, Boolean>();
        while (iter.hasNext()) {
            analysers.put(iter.next(), Boolean.TRUE);
        }

        //Create GUI
        toolbar = new JToolBar();
        analyseButton = new JButton();
        configureAnalysersButton = new JButton();
        scrollPane = new CheckerboardScrollPane(new ImagePanel(image.image()));

        setLayout(new BorderLayout());

        toolbar.setRollover(true);

        analyseButton.setIcon(
                new ImageIcon(getClass().getResource("/org/purview/analysers/frontend/gears.png")));
        Mnemonics.setLocalizedText(analyseButton, analyseButtonText);
        analyseButton.setFocusable(false);
        analyseButton.setHorizontalTextPosition(SwingConstants.CENTER);
        analyseButton.setVerticalTextPosition(SwingConstants.BOTTOM);
        analyseButton.addActionListener(this);
        toolbar.add(analyseButton);

        configureAnalysersButton.setIcon(new ImageIcon(getClass().getResource("/org/purview/analysers/frontend/settings.png")));
        Mnemonics.setLocalizedText(configureAnalysersButton, configureAnalysersButtonText);
        configureAnalysersButton.setFocusable(false);
        configureAnalysersButton.setHorizontalTextPosition(SwingConstants.CENTER);
        configureAnalysersButton.setVerticalTextPosition(SwingConstants.BOTTOM);
        configureAnalysersButton.addActionListener(this);
        toolbar.add(configureAnalysersButton);

        add(toolbar, BorderLayout.PAGE_START);
        add(scrollPane, BorderLayout.CENTER);

        setName(NbBundle.getMessage(ImageSessionTopComponent.class, "CTL_ImageSessionTopComponent", imageFile.getName()));
        setToolTipText(componentTooltip);
        setIcon(componentIcon);
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
            //Create a list of (active) analysers to use
            LinkedList<Analyser<ImageMatrix>> activeAnalysers = new LinkedList<Analyser<ImageMatrix>>();
            for (Analyser<ImageMatrix> key : analysers.keySet()) {
                if (analysers.get(key)) {
                    activeAnalysers.add(key);
                }
            }

            //Create a new analysis top component with the given analysers and image
            AnalysisTopComponent analysisComponent = new AnalysisTopComponent(name,
                    image, activeAnalysers);
            analysisComponent.open();
            analysisComponent.requestActive();

            //Start the analysis on another thread
            Thread analysisThread = new Thread(analysisComponent);
            analysisThread.setName("Analyser thread for image \"" + name + "\"");
            analysisThread.start();
        } else if (e.getSource() == configureAnalysersButton) {
            //Open a new analyser settings dialog
            Frame window = WindowManager.getDefault().getMainWindow();
            AnalyserSettingsDialog dialog = new AnalyserSettingsDialog(analysers, window, true);
            dialog.setVisible(true);
        }
    }
}
