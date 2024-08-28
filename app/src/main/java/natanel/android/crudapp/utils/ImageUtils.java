package natanel.android.crudapp.utils;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class ImageUtils {

    public static void saveImageToInternalStorage(@NonNull Context context, Uri imageUri, String fileName) {
        try {
            InputStream inputStream = context.getContentResolver().openInputStream(imageUri);
            Bitmap bitmap = BitmapFactory.decodeStream(inputStream);
            File file = new File(context.getFilesDir(), fileName);
            FileOutputStream fos = new FileOutputStream(file);
            bitmap.compress(Bitmap.CompressFormat.JPEG, 100, fos);
            fos.close();
        } catch (IOException e) {
            Toast.makeText(context, "Failed to save image: " + e.getMessage(), Toast.LENGTH_SHORT).show();
        }
    }

    public static void removeImageFromStorage(@NonNull Context context, String fileName) {
        File file = new File(context.getFilesDir(), fileName);
        if (file.exists() && file.delete()) {
            Toast.makeText(context, "Image removed", Toast.LENGTH_SHORT).show();
        } else {
            Toast.makeText(context, "Failed to remove image", Toast.LENGTH_SHORT).show();
        }
    }
}
