package org.purview.analysers.frontend;

import com.drew.imaging.jpeg.JpegMetadataReader;
import com.drew.imaging.jpeg.JpegProcessingException;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.MetadataException;
import com.drew.metadata.Tag;
import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import org.openide.awt.Mnemonics;
import org.openide.util.Exceptions;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import org.purview.analysers.DefaultAnalysers;
import org.purview.core.analysis.Analyser;
import org.purview.core.data.ImageMatrix;
import scala.Function0;
import scala.Tuple2;
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
    private final BufferedImage image;
    private final String name;
    //Some needed analyser-related variables
    private final Map<Analyser<ImageMatrix>, Boolean> analysers;
    private scala.collection.immutable.Map<String, scala.collection.immutable.Map<String, String>> metadata;

    ImageSessionTopComponent(final File imageFile) throws IOException {
        //Load the image
        ImageInputStream stream = ImageIO.createImageInputStream(imageFile);
        BufferedImage img = null;

        //Try to load the image somehow
        try {
            java.util.Iterator<ImageReader> readers = ImageIO.getImageReaders(stream);

            if (!readers.hasNext()) {
                throw new IllegalArgumentException("No readers found for the specified image");
            }

            img = readImageAndMetadata(readers.next(), stream, imageFile);
        } finally {
            stream.close();
        }

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
        //TODO: use modules!
        Seq<Function0<Analyser<ImageMatrix>>> analyserFactories = DefaultAnalysers.analysers();
        Iterator<Function0<Analyser<ImageMatrix>>> iter = analyserFactories.iterator();
        HashMap<Analyser<ImageMatrix>, Boolean> createdAnalysers =
                new HashMap<Analyser<ImageMatrix>, Boolean>();

        while (iter.hasNext()) {
            Function0<Analyser<ImageMatrix>> c = iter.next();
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

    private BufferedImage readImageAndMetadata(final ImageReader reader,
            final ImageInputStream stream, final File imageFile)
            throws IOException {
        //Set up the read head
        reader.setInput(stream);

        //Create a new map for the metadata (MODULE$ accesses the singleton in Java)
        metadata = scala.collection.immutable.Map$.MODULE$.empty();

        //We can only read metadata via the JPEG format at the moment
        if ("JPEG".equals(reader.getFormatName())) {
            try {
                final Metadata meta = JpegMetadataReader.readMetadata(imageFile);

                //Traverse the metadata shallow tree
                final java.util.Iterator dirs = meta.getDirectoryIterator();
                while (dirs.hasNext()) {
                    final Directory dir = (Directory) dirs.next();
                    if (null != dir && null != dir.getName()) {
                        final java.util.Iterator tags = dir.getTagIterator();
                        scala.collection.immutable.Map<String, String> dirMeta =
                                scala.collection.immutable.Map$.MODULE$.empty();
                        while (tags.hasNext()) {
                            Tag tag = (Tag) tags.next();
                            if (null != tag && null != tag.getDescription() && null != tag.getTagName()) {
                                dirMeta = dirMeta.$plus(new Tuple2<String, String>(tag.getTagName(), tag.getDescription()));
                            }
                        }

                        //Add the metadata entry
                        metadata = metadata.$plus(
                                new Tuple2<String, scala.collection.immutable.Map<String, String>>(dir.getName(), dirMeta));
                    }
                }
            } catch (JpegProcessingException ex) {
                Exceptions.printStackTrace(ex);
            } catch (MetadataException ex) {
                Exceptions.printStackTrace(ex);
            }
        }

        //Actually read the image data:
        return reader.read(0);
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
                    new ImageMatrix(image, metadata), activeAnalysers);
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
