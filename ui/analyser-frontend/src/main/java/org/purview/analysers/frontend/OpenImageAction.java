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

public final class OpenImageAction extends CallableSystemAction {

    private final JFileChooser imageChooser = new JFileChooser();

    public OpenImageAction() {
        imageChooser.setDialogTitle(NbBundle.getMessage(OpenImageAction.class, "MSG_OpenImage"));
        imageChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        imageChooser.setFileHidingEnabled(true);
        imageChooser.setFileFilter(new FileFilter() {

            private String[] formats = ImageIO.getReaderFileSuffixes();

            @Override
            public boolean accept(File file) {
                boolean supported = false;
                for (String suffix : formats) {
                    if (file.getName().endsWith("." + suffix)) {
                        supported = true;
                    }
                }
                return file.isDirectory() || supported;
            }

            @Override
            public String getDescription() {
                return "Supported image files";
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
        return NbBundle.getMessage(OpenImageAction.class, "CTL_OpenImageAction");
    }

    @Override
    protected String iconResource() {
        return "org/purview/ui/analyse/open.png";
    }

    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }

    @Override
    protected boolean asynchronous() {
        return false;
    }
}
