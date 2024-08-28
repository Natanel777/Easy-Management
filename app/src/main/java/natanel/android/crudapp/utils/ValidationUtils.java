package natanel.android.crudapp.utils;

import android.util.Patterns;

public class ValidationUtils {

    // Validate the name
    public static boolean validateName(String name) {
        // Split the name into first and last names
        String[] nameParts = name.trim().split(" ", 2);
        return nameParts.length >= 2 && !nameParts[0].isEmpty() && !nameParts[1].isEmpty();
    }

    // Validate the email
    public static boolean validateEmail(String email) {
        return Patterns.EMAIL_ADDRESS.matcher(email).matches();
    }

    // Capitalize the first letter of each part of the name
    public static String capitalizeName(String name) {
        StringBuilder capitalized = new StringBuilder();
        String[] parts = name.split(" ");
        for (String part : parts) {
            if (!part.isEmpty()) {
                capitalized.append(Character.toUpperCase(part.charAt(0)))
                        .append(part.substring(1).toLowerCase())
                        .append(" ");
            }
        }
        return capitalized.toString().trim();
    }
}
