package algs.meta_heuristics.util.gif;

import java.awt.image.*;
import java.util.Random;

/**
 * User: saviderek
 * Date: 1/13/14
 */
public class AnimatedGifExample {
    public static void main(String[] args) {
        int[] pixels = new int[200*200*3];
        Random rng = new Random(System.currentTimeMillis());
        for (int i=0;i < 200*200*3;i++) {

            pixels[i] = rng.nextInt(256);
        }
        BufferedImage image = new BufferedImage(200, 200, BufferedImage.TYPE_INT_RGB);
        WritableRaster raster = (WritableRaster) image.getRaster();
        raster.setPixels(0, 0, 200, 200, pixels);

        for (int i=0;i < 200*200*3;i++) {

            pixels[i] = rng.nextInt(256);
        }
        BufferedImage image2 = new BufferedImage(200, 200, BufferedImage.TYPE_INT_RGB);
        raster = (WritableRaster) image2.getRaster();
        raster.setPixels(0, 0, 200, 200, pixels);


        AnimatedGifEncoder e = new AnimatedGifEncoder();
        e.start("temp.gif");
        e.setDelay(1000);   // 1 frame per sec
        e.setRepeat(0);
        e.addFrame(image);
        e.addFrame(image2);
        e.finish();
    }

}
