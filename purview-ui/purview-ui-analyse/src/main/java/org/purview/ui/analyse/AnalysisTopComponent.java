package org.purview.ui.analyse;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.image.BufferedImage;
import java.util.LinkedList;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import org.openide.awt.Mnemonics;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;
import org.purview.analysers.DefaultAnalysers;
import org.purview.core.analysis.Analyser;
import org.purview.core.data.Color;
import org.purview.core.data.Matrix;
import org.purview.core.data.MatrixOps;
import org.purview.core.session.AnalysisSession;
import org.purview.core.session.AnalysisStats;
import scala.Function0;
import scala.collection.Iterator;
import scala.collection.JavaConversions;
import scala.collection.Seq;

/**
 * Top component which displays something.
 */
final class AnalysisTopComponent extends TopComponent implements Runnable {

    private static final String ICON_PATH = "org/purview/ui/analyse/analyse.png";
    private static final String PREFERRED_ID = "AnalysisComponent";
    private final JLabel analyserDescrLabel = new JLabel();
    private final JLabel analyserLabel = new JLabel();
    private final JScrollPane listScroller = new JScrollPane();
    private final JPanel mainPanel = new JPanel();
    private final JProgressBar progressBar = new JProgressBar();
    private final JLabel stageDescrLabel = new JLabel();
    private final JLabel stageLabel = new JLabel();
    private final DefaultListModel model = new DefaultListModel();
    private final JList statusList = new JList();
    private final AnalysisSession session;

    public AnalysisTopComponent(final String imageName, final BufferedImage image) {
        initComponents();
        setName(NbBundle.getMessage(AnalysisTopComponent.class, "CTL_AnalysisTopComponent", imageName));
        setToolTipText(NbBundle.getMessage(AnalysisTopComponent.class, "HINT_AnalysisTopComponent"));
        setIcon(ImageUtilities.loadImage(ICON_PATH, true));

        //Do some really nifty Scala interop conversions!
        Seq<Function0<Analyser<Matrix<Color>>>> analysers = DefaultAnalysers.analysers();
        Iterator<Function0<Analyser<Matrix<Color>>>> iter = analysers.iterator();
        LinkedList<Analyser<Matrix<Color>>> result = new LinkedList<Analyser<Matrix<Color>>>();

        while (iter.hasNext()) {
            Function0<Analyser<Matrix<Color>>> c = iter.next();
            result.add(c.apply());
        }

        session = new AnalysisSession<Matrix<Color>>(
                JavaConversions.asBuffer(result).toSeq(),
                MatrixOps.imageToMatrix(image));
    }

    public void run() {
        AnalysisStats stats = new AnalysisStats() {

            @Override
            public void reportProgress(final float progress) {
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        progressBar.setValue((int) (10000 * progress));
                    }
                });
            }

            @Override
            public void reportStatus(final String status) {
                model.addElement(status);
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        statusList.repaint();
                    }
                });
            }

            @Override
            public void reportStage(final String stage) {
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        stageLabel.setText(stage);
                    }
                });
            }

            @Override
            public void reportAnalyser(final String stage) {
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        analyserLabel.setText(stage);
                    }
                });
            }
        };
        session.run(stats);
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     */
    private void initComponents() {

        setLayout(new BorderLayout());
        add(progressBar, BorderLayout.PAGE_END);

        mainPanel.setLayout(new GridLayout(2, 2));

        Mnemonics.setLocalizedText(analyserDescrLabel, NbBundle.getMessage(AnalysisTopComponent.class, "LBL_Analyser"));
        mainPanel.add(analyserDescrLabel);

        mainPanel.add(analyserLabel);

        Mnemonics.setLocalizedText(stageDescrLabel, NbBundle.getMessage(AnalysisTopComponent.class, "LBL_Stage"));
        mainPanel.add(stageDescrLabel);

        mainPanel.add(stageLabel);

        add(mainPanel, BorderLayout.PAGE_START);

        listScroller.setViewportView(statusList);

        add(listScroller, BorderLayout.CENTER);
    }

    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_NEVER;
    }

    @Override
    protected String preferredID() {
        return PREFERRED_ID;
    }
}
