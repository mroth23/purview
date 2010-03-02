package org.purview.ui.analyse;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import org.openide.awt.Mnemonics;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.purview.core.analysis.Analyser;
import org.purview.core.data.Color;
import org.purview.core.data.Matrix;
import org.purview.core.data.MatrixOps;
import org.purview.core.report.ReportEntry;
import org.purview.core.session.AnalysisSession;
import org.purview.core.session.AnalysisStats;
import scala.collection.Iterator;
import scala.collection.JavaConversions;
import scala.collection.Set;

/**
 * Top component which displays something.
 */
final class AnalysisTopComponent extends TopComponent implements Runnable {

    private static final String ICON_PATH = "org/purview/ui/analyse/analyse.png";
    private static final String PREFERRED_ID = "AnalysisTopComponent";
    private final JLabel analyserDescrLabel = new JLabel();
    private final JLabel analyserLabel = new JLabel();
    private final JLabel stageDescrLabel = new JLabel();
    private final JLabel stageLabel = new JLabel();
    private final JScrollPane listScroller = new JScrollPane();
    private final JPanel mainPanel = new JPanel();
    private final JProgressBar progressBar = new JProgressBar();
    private final JTextArea statusList = new JTextArea();
    private final AnalysisSession session;
    private final BufferedImage image;
    private final String imageName;

    public AnalysisTopComponent(final String imageName, final BufferedImage image,
            final List<Analyser<Matrix<Color>>> analysers) {
        initComponents();
        setName(NbBundle.getMessage(AnalysisTopComponent.class, "CTL_AnalysisTopComponent", imageName));
        setToolTipText(NbBundle.getMessage(AnalysisTopComponent.class, "HINT_AnalysisTopComponent"));
        setIcon(ImageUtilities.loadImage(ICON_PATH, true));

        final Matrix<Color> matrix = MatrixOps.imageToMatrix(image);
        session = new AnalysisSession<Matrix<Color>>(
                JavaConversions.asBuffer(analysers).toSeq(),
                matrix);
        this.image = image;
        this.imageName = imageName;
    }

    public void run() {
        AnalysisStats stats = new AnalysisStats() {

            private int oldProgress = 0;

            @Override
            public void reportProgress(final float progress) {
                final int actualProgress = (int) (1000 * progress);
                if (actualProgress > oldProgress) {
                    SwingUtilities.invokeLater(new Runnable() {

                        public void run() {
                            progressBar.setValue(actualProgress);
                        }
                    });
                    oldProgress = actualProgress;
                }
            }

            @Override
            public void reportStatus(final String status) {
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        statusList.append(status + '\n');
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
        final scala.collection.Map<Analyser<Matrix<Color>>, Set<ReportEntry>> results = session.run(stats);
        final Iterator<Analyser<Matrix<Color>>> analyserIter = results.keySet().iterator();
        final Map<Analyser<Matrix<org.purview.core.data.Color>>, List<ReportEntry>> report = new HashMap<Analyser<Matrix<org.purview.core.data.Color>>, List<ReportEntry>>();

        while (analyserIter.hasNext()) {
            final Analyser<Matrix<Color>> analyser = analyserIter.next();
            final Set<ReportEntry> resultsForAnalyser = results.apply(analyser);
            final Iterator<ReportEntry> reportIter = resultsForAnalyser.iterator();
            final LinkedList<ReportEntry> entries = new LinkedList<ReportEntry>();

            while (reportIter.hasNext()) {
                entries.add(reportIter.next());
            }
            report.put(analyser, entries);
        }

        SwingUtilities.invokeLater(new Runnable() {

            public void run() {
                final ResultsTopComponent resultsComp = new ResultsTopComponent(imageName, image, report);
                resultsComp.open();
                resultsComp.requestActive();
            }
        });
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     */
    private void initComponents() {

        setLayout(new BorderLayout());

        progressBar.setMinimum(0);
        progressBar.setMaximum(1000);

        add(progressBar, BorderLayout.PAGE_END);

        mainPanel.setLayout(new GridLayout(2, 1));

        Mnemonics.setLocalizedText(analyserDescrLabel, NbBundle.getMessage(AnalysisTopComponent.class, "LBL_Analyser") + ": ");

        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.LINE_AXIS));
        container.add(analyserDescrLabel);
        container.add(analyserLabel);

        mainPanel.add(container);

        Mnemonics.setLocalizedText(stageDescrLabel, NbBundle.getMessage(AnalysisTopComponent.class, "LBL_Stage") + ": ");

        container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.LINE_AXIS));
        container.add(stageDescrLabel);
        container.add(stageLabel);

        mainPanel.add(container);

        add(mainPanel, BorderLayout.PAGE_START);

        statusList.setEditable(false);
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

    @Override
    public List<Mode> availableModes(final List<Mode> input) {
        final LinkedList<Mode> result = new LinkedList<Mode>();
        for (final Mode mode : input) {
            if (mode.getName().equals("output") || mode.getName().endsWith("NewMode")) {
                result.add(mode);
            }
        }
        return result;
    }
}
