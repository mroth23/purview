package org.purview.analysers.frontend;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import org.openide.util.Exceptions;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;
import org.purview.core.analysis.Analyser;
import org.purview.core.analysis.Metadata;
import org.purview.core.data.ImageMatrix;
import org.purview.core.report.Circle;
import org.purview.core.report.FreeShape;
import org.purview.core.report.FreeSourceShape;
import org.purview.core.report.Image;
import org.purview.core.report.LevelColor;
import org.purview.core.report.Point;
import org.purview.core.report.Rectangle;
import org.purview.core.report.ReportEntry;
import org.purview.core.report.ReportLevel;
import org.purview.core.report.SourceCircle;
import org.purview.core.report.SourcePoint;
import org.purview.core.report.SourceRectangle;

/**
 * Top component which displays something.
 */
final class ResultsTopComponent extends TopComponent implements TreeSelectionListener {

    private static final String ICON_PATH = "org/purview/analysers/frontend/done.png";
    private static final String PREFERRED_ID = "ResultsTopComponent";
    private final JSplitPane splitter;
    private final JTree reportTree;
    private final JScrollPane reportTreeScroller;
    private final ReportPanel reportPanel;
    private final Map<TreeNode, ReportEntry> callbacks;

    public ResultsTopComponent(final String imageName, final BufferedImage image,
            final Map<Analyser<ImageMatrix>, List<ReportEntry>> report) {
        setName(NbBundle.getMessage(ResultsTopComponent.class, "CTL_ResultsTopComponent", imageName));
        setToolTipText(NbBundle.getMessage(ResultsTopComponent.class, "HINT_ResultsTopComponent"));
        setIcon(ImageUtilities.loadImage(ICON_PATH, true));

        reportTreeScroller = new JScrollPane();

        final DefaultMutableTreeNode rootNode =
                new DefaultMutableTreeNode(NbBundle.getMessage(ResultsTopComponent.class, "LBL_Report"));

        int unknownAnalyserIdx = 0;
        callbacks = new HashMap<TreeNode, ReportEntry>();
        for (final Analyser<ImageMatrix> analyser : report.keySet()) {
            //TODO: should we sync the "unknown index" with the one from the settings dialog?
            final DefaultMutableTreeNode analyserNode = (analyser instanceof Metadata)
                    ? new DefaultMutableTreeNode(((Metadata) analyser).name())
                    : new DefaultMutableTreeNode(NbBundle.getMessage(ResultsTopComponent.class,
                    "LBL_UnknownAnalyser", ++unknownAnalyserIdx));
            //TODO: add tooltips, etc

            for (final ReportEntry entry : report.get(analyser)) {
                final DefaultMutableTreeNode entryNode = new DefaultMutableTreeNode(entry.toString());
                callbacks.put(entryNode, entry);
                analyserNode.add(entryNode);
            }
            rootNode.add(analyserNode);
        }

        setLayout(new java.awt.BorderLayout());

        splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

        reportTree = new JTree(rootNode);
        reportTree.addTreeSelectionListener(this);
        reportTree.setCellRenderer(new ReportEntryTreeCellRenderer(callbacks));
        reportTreeScroller.setViewportView(reportTree);
        splitter.add(reportTreeScroller, JSplitPane.LEFT);

        reportPanel = new ReportPanel(image);
        reportPanel.setOpaque(false);
        splitter.add(new CheckerboardScrollPane(reportPanel), JSplitPane.RIGHT);
        add(splitter, BorderLayout.CENTER);
    }

    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_NEVER;
    }

    @Override
    protected String preferredID() {
        return PREFERRED_ID;
    }

    public void valueChanged(TreeSelectionEvent e) {
        if (e.getSource() == reportTree) { //You never know...
            TreePath path = e.getPath();
            TreeNode node = (TreeNode) path.getLastPathComponent();
            if (node.isLeaf() && callbacks.containsKey(node)) {
                ReportEntry entry = callbacks.get(node);
                reportPanel.setReportEntry(entry);

                if (entry instanceof Point) {
                    Point p = (Point) entry;
                    if (entry instanceof Rectangle) {
                        Rectangle r = (Rectangle) entry;
                        reportPanel.scrollRectToVisible(
                                new java.awt.Rectangle(p.x(), p.y(),
                                r.width(), r.height()));
                    } else if (entry instanceof Circle) {
                        Circle c = (Circle) entry;
                        reportPanel.scrollRectToVisible(
                                new java.awt.Rectangle(p.x() - c.radius(), p.y() - c.radius(),
                                c.radius() * 2, c.radius() * 2));
                    } else if (!(entry instanceof Image)) {
                        reportPanel.scrollRectToVisible(new java.awt.Rectangle(p.x(), p.y(), 0, 0));
                    }
                }
                reportPanel.repaint();
            }
        }
    }
}

class ReportEntryTreeCellRenderer extends DefaultTreeCellRenderer {

    private final Map<TreeNode, ReportEntry> callbacks;
    private static final Icon nodeIcon = ImageUtilities.loadImageIcon("org/purview/analysers/frontend/done.png", true);
    private static final Map<ReportEntry, Icon> icons = new HashMap<ReportEntry, Icon>();
    private static final Ellipse2D.Float circle = new Ellipse2D.Float(4, 4, 8, 8);
    private static final Rectangle2D.Float rectangle = new Rectangle2D.Float(4, 4, 8, 8);
    private static final Path2D.Float triangle = new Path2D.Float();
    private static final Ellipse2D.Float point = new Ellipse2D.Float(6, 6, 4, 4);
    private final Stroke stroke = new BasicStroke(2);

    static {
        triangle.moveTo(4, 4);
        triangle.lineTo(11, 8);
        triangle.lineTo(4, 12);
        triangle.closePath();
    }

    public ReportEntryTreeCellRenderer(final Map<TreeNode, ReportEntry> callbacks) {
        this.callbacks = callbacks;
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
        if (leaf) {
            if (value instanceof TreeNode && callbacks.containsKey((TreeNode) value)) {
                final ReportEntry entry = callbacks.get((TreeNode) value);
                final ReportLevel level = entry.level();
                final Color color = (level instanceof LevelColor)
                        ? ((LevelColor) level).color().toAWTColor()
                        : Color.black;

                if (!icons.containsKey(entry)) {
                    BufferedImage img = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
                    Graphics2D g = img.createGraphics();
                    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                            RenderingHints.VALUE_ANTIALIAS_ON);

                    if (entry instanceof Image) {
                        Image i = (Image) entry;
                        g.drawImage(i.image(), 1, 1, 14, 14, tree);
                    } else {
                        g.setStroke(stroke);

                        final Color background = new Color(color.getRed(), color.getGreen(), color.getBlue(), color.getAlpha() / 2);

                        Shape shape = null;
                        if (entry instanceof SourcePoint) {
                            shape = triangle;
                        } else if (entry instanceof Rectangle) {
                            shape = rectangle;
                        } else if (entry instanceof Circle) {
                            shape = circle;
                        } else {
                            shape = point;
                        }

                        g.setPaint(background);
                        g.fill(shape);
                        g.setPaint(color);
                        g.draw(shape);
                    }

                    g.dispose();
                    icons.put(entry, new ImageIcon(img));
                }

                this.setIcon(icons.get(entry));
            } else {
                this.setIcon(null); //Default icon
            }
        } else {
            this.setIcon(nodeIcon);
        }
        return this;
    }
}

class ReportPanel extends JPanel implements Runnable {

    public static final float POINT_RADIUS = 2f;
    public static final float STROKE_WIDTH = 2f;
    public static final float ARROW_WIDTH = 12f;
    public static final double ARROW_ANGLE = Math.toRadians(20);
    public static final Stroke stroke = new BasicStroke(STROKE_WIDTH, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
    public static final Stroke squareStroke = new BasicStroke(STROKE_WIDTH, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER);
    public static final Color sourceColor = Color.blue;
    public static final Color arrowColor = Color.white;
    private volatile boolean hasUpdater = false;
    private final BufferedImage image;
    private ReportEntry entry = null;

    public ReportPanel(final BufferedImage image) {
        this.image = image;
        setPreferredSize(new Dimension(image.getWidth(), image.getHeight()));
    }

    public void setReportEntry(final ReportEntry entry) {
        if (null != entry) {
            new Thread(this).start();
        }
        this.entry = entry;
    }

    public ReportEntry getReportEntry() {
        return entry;
    }

    private void makePoint(Graphics2D g, float x, float y, Color fill, Color outline) {
        final Ellipse2D.Float point = new Ellipse2D.Float(x - POINT_RADIUS, y - POINT_RADIUS, POINT_RADIUS * 2, POINT_RADIUS * 2);
        makeShape(g, point, fill, outline);
    }

    private void makeShape(Graphics2D g, Shape shape, Color fill, Color outline) {
        //Fill for shape
        g.setPaint(fill);
        g.fill(shape);

        //Outline for shape
        g.setPaint(outline);
        g.draw(shape);
    }

    private void makeArrow(Graphics2D g, float x1, float y1, float x2, float y2, Color color) {
        final Path2D.Float path = new Path2D.Float();

        final float dx = x2 - x1;
        final float dy = y2 - y1;
        final double theta = Math.atan2(dy, dx) + Math.PI;

        final float mx = (float) (x2 + ARROW_WIDTH / 2 * Math.cos(theta));
        final float my = (float) (y2 + ARROW_WIDTH / 2 * Math.sin(theta));
        final double ax1 = x2 + ARROW_WIDTH * Math.cos(theta + ARROW_ANGLE);
        final double ay1 = y2 + ARROW_WIDTH * Math.sin(theta + ARROW_ANGLE);
        final double ax2 = x2 + ARROW_WIDTH * Math.cos(theta - ARROW_ANGLE);
        final double ay2 = y2 + ARROW_WIDTH * Math.sin(theta - ARROW_ANGLE);

        path.moveTo(x2, y2);
        path.lineTo(ax1, ay1);
        path.lineTo(ax2, ay2);
        path.closePath();

        final Line2D.Float arrowLine = new Line2D.Float(x1, y1, mx, my);

        //Outline for arrow
        Stroke prev = g.getStroke();
        g.setStroke(squareStroke);
        g.setPaint(color);
        g.draw(arrowLine);
        g.fill(path);
        g.setStroke(prev);
    }

    @Override
    public void paint(Graphics gr) {
        //Let's use a more modern graphics API
        final Graphics2D graphics = (Graphics2D) gr;

        //Fade between 0.2f and 0.6f every half second
        float phase = 0.2f + 0.4f * (float) Math.abs(Math.sin(System.currentTimeMillis() / (Math.PI * 125)));

        graphics.drawImage(image, 0, 0, this);

        if (null != entry) {
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);

            final ReportLevel level = entry.level();

            final Color color = (level instanceof LevelColor)
                    ? ((LevelColor) level).color().toAWTColor() : Color.red;
            final Color transpColor = new Color(color.getRed(), color.getGreen(),
                    color.getBlue(), (int) (color.getAlpha() * phase));
            final Color transpSourceColor = new Color(sourceColor.getRed(), sourceColor.getGreen(),
                    sourceColor.getBlue(), (int) (sourceColor.getAlpha() * phase));

            graphics.setStroke(stroke);

            //TODO: This could be put into a ReportEntry trait, but should it be? Is this too implementation specific?
            if (entry instanceof Point) {
                final Point point = (Point) entry;

                if (!(entry instanceof Image)) {
                    makePoint(graphics, point.x(), point.y(), transpColor, color);
                }

                boolean hasArrow = false;
                float arrowSourceX = 0, arrowSourceY = 0;

                if (entry instanceof SourcePoint) {
                    final SourcePoint sourcePoint = (SourcePoint) entry;
                    makePoint(graphics, sourcePoint.sourceX(), sourcePoint.sourceY(),
                            transpSourceColor, sourceColor);

                    if (entry instanceof FreeSourceShape) {
                        final FreeSourceShape sourceShape = (FreeSourceShape) entry;
                        makeShape(graphics, sourceShape.sourceShape(),
                                transpSourceColor, sourceColor);
                    }
                    arrowSourceX = sourcePoint.sourceX();
                    arrowSourceY = sourcePoint.sourceY();
                    hasArrow = true;
                }

                if (entry instanceof FreeShape) {
                    final FreeShape shape = (FreeShape) entry;
                    makeShape(graphics, shape.shape(),
                            transpColor, color);
                }
                if (entry instanceof Image) {
                    final Image img = (Image) entry;
                    graphics.drawImage(img.image(), point.x(), point.y(), this);
                }

                if (hasArrow) {
                    makeArrow(graphics, arrowSourceX, arrowSourceY,
                            point.x(), point.y(),
                            arrowColor);
                }
            }
        }
    }

    @Override
    public void setVisible(boolean visible) {
        if (visible) {
            new Thread(this).start();
        }

        super.setVisible(visible);
    }

    public void run() {
        if (!hasUpdater) {
            hasUpdater = true;
            while (null != entry && isVisible()) {
                if (entry instanceof Point) {
                    int x, y, width, height;
                    final Point point = (Point) entry;
                    if (entry instanceof FreeShape) {
                        final Rectangle2D bounds = ((FreeShape) entry).shape().getBounds2D();
                        x = (int) bounds.getX();
                        y = (int) bounds.getY();
                        width = (int) bounds.getWidth();
                        height = (int) bounds.getHeight();
                    } else if (entry instanceof Image) {
                        final Image entryImage = (Image) entry;
                        x = point.x();
                        y = point.y();
                        width = point.x() + entryImage.image().getWidth();
                        height = point.y() + entryImage.image().getHeight();
                    } else {
                        x = point.x();
                        y = point.y();
                        width = 0;
                        height = 0;
                    }
                    final Rectangle2D rect = new Rectangle2D.Float(x, y, width, height);
                    
                    if (entry instanceof SourcePoint) {
                        final SourcePoint sourcePoint = (SourcePoint) entry;
                        int sourceX, sourceY, sourceWidth, sourceHeight;

                        if (entry instanceof FreeSourceShape) {
                            final Rectangle2D bounds = ((FreeSourceShape) entry).sourceShape().getBounds2D();
                            sourceX = (int) bounds.getX();
                            sourceY = (int) bounds.getY();
                            sourceWidth = (int) bounds.getWidth();
                            sourceHeight = (int) bounds.getHeight();
                        } else {
                            sourceX = sourcePoint.sourceX();
                            sourceY = sourcePoint.sourceY();
                            sourceWidth = 0;
                            sourceHeight = 0;
                        }
                        Rectangle2D sourceRect = new Rectangle2D.Float(sourceX, sourceY, sourceWidth, sourceHeight);
                        rect.add(sourceRect);
                    }

                    this.repaint((int)rect.getX(), (int)rect.getY(), (int)rect.getWidth(), (int)rect.getHeight());
                }

                try {
                    Thread.sleep(20);
                } catch (InterruptedException ex) {
                    Exceptions.printStackTrace(ex);
                }
            }
            hasUpdater = false;
        }
    }
}
