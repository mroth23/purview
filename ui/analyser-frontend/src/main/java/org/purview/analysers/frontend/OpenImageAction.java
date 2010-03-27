package org.purview.analysers.frontend;

import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import org.openide.util.Exceptions;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallableSystemAction;

/**
 * Opens a new image file and creates an ImageSession for it
 */
public final class OpenImageAction extends CallableSystemAction {
    private static final String actionName = NbBundle.getMessage(OpenImageAction.class, "CTL_OpenImageAction");
    private static final JFileChooser imageChooser = new JFileChooser();

    static {
        //Set up the chooser
        imageChooser.setDialogTitle(NbBundle.getMessage(OpenImageAction.class, "MSG_OpenImage"));
        imageChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        imageChooser.setFileHidingEnabled(true);

        //Add filters for all known image formats
        imageChooser.setFileFilter(new FileFilter() {

            private final String[] formats = ImageIO.getReaderFileSuffixes();

            @Override
            public boolean accept(final File file) {
                if (file.isDirectory()) {
                    return true;
                } else {
                    boolean supported = false;
                    for (final String suffix : formats) {
                        if (file.getName().endsWith("." + suffix)) {
                            supported = true;
                        }
                    }
                    return supported;
                }
            }

            @Override
            public String getDescription() {
                return "Supported image files"; //TODO: translate
            }
        });
    }

    public void performAction() {
        if (imageChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION
                && imageChooser.getSelectedFile() != null) {
            try {
                ImageSessionTopComponent tc =
                        new ImageSessionTopComponent(imageChooser.getSelectedFile());
                tc.open();
                tc.requestActive();
            } catch (IOException ex) {
                Exceptions.printStackTrace(ex);
            }
        }
    }

    public String getName() {
        return actionName;
    }

    @Override
    protected String iconResource() {
        return "org/purview/analysers/frontend/open.png";
    }

    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }

    @Override
    protected boolean asynchronous() {
        return false;
    }
}
