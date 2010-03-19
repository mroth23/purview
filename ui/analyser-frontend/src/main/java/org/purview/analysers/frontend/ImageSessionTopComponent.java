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
    private final Map<Analyser<ImageMatrix>, Boolean> analysers;
    private scala.collection.immutable.Map<String, scala.collection.immutable.Map<String, String>> metadata;

    ImageSessionTopComponent(final File imageFile) throws IOException {
        //Load the image
        ImageInputStream stream = ImageIO.createImageInputStream(imageFile);
        BufferedImage img = null;

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

    private BufferedImage readImageAndMetadata(final ImageReader reader,
            final ImageInputStream stream, final File imageFile)
            throws IOException {
        reader.setInput(stream);
        metadata = scala.collection.immutable.Map$.MODULE$.empty();
        if ("JPEG".equals(reader.getFormatName())) {
            System.out.println("Image is a JPEG image; will read metadata");
            try {
                final Metadata meta = JpegMetadataReader.readMetadata(imageFile);
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
                        metadata = metadata.$plus(
                                new Tuple2<String, scala.collection.immutable.Map<String, String>>(dir.getName(), dirMeta));
                    }
                }
            } catch (JpegProcessingException ex) {
                Exceptions.printStackTrace(ex);
            } catch (MetadataException ex) {
                Exceptions.printStackTrace(ex);
            }
        } else {
            System.out.println("Image has format " + reader.getFormatName());
        }
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
            LinkedList<Analyser<ImageMatrix>> activeAnalysers = new LinkedList<Analyser<ImageMatrix>>();
            for (Analyser<ImageMatrix> key : analysers.keySet()) {
                if (analysers.get(key)) {
                    activeAnalysers.add(key);
                }
            }
            AnalysisTopComponent c = new AnalysisTopComponent(name,
                    new ImageMatrix(image, metadata), activeAnalysers);
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
