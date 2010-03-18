package org.purview.lookandfeel;

import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;
import org.openide.modules.ModuleInstall;

public class LookAndFeelModule extends ModuleInstall {

    @Override
    public void restored() {
        try {
            for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            //ignore
        } catch (InstantiationException ex) {
            //ignore
        } catch (IllegalAccessException ex) {
            //ignore
        } catch (UnsupportedLookAndFeelException ex) {
            //ignore
        }
    }
}
