package natanel.android.crudapp.adapter;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.provider.MediaStore;
import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import jp.wasabeef.picasso.transformations.BlurTransformation;
import natanel.android.crudapp.MainActivity;
import natanel.android.crudapp.R;
import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.databinding.ItemUserBinding;
import natanel.android.crudapp.ui.UserListFragment;
import natanel.android.crudapp.ui.UserListViewModel;
import natanel.android.crudapp.utils.ImageUtils;
import natanel.android.crudapp.utils.ValidationUtils;

public class UserAdapter extends RecyclerView.Adapter<UserAdapter.UserViewHolder> {

    private List<User> users = new ArrayList<>();
    private final UserListViewModel viewModel; // Reference to ViewModel

    private final UserListFragment fragment; // Store the fragment reference

    private final ActivityResultLauncher<Intent> imagePickerLauncher;

    public UserAdapter(UserListViewModel viewModel, UserListFragment fragment, ActivityResultLauncher<Intent> imagePickerLauncher) {
        this.viewModel = viewModel;
        this.fragment = fragment;
        this.imagePickerLauncher = imagePickerLauncher;
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setUsers(List<User> users) {
        this.users = users;
        notifyDataSetChanged();
    }


    @NonNull
    @Override
    public UserViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        ItemUserBinding binding = ItemUserBinding.inflate(inflater, parent, false);
        return new UserViewHolder(binding, viewModel,imagePickerLauncher,fragment);
    }

    @Override
    public int getItemCount() {
        return users.size();
    }


    @Override
    public void onBindViewHolder(@NonNull UserViewHolder holder, int position) {
        User user = users.get(position);
        holder.bind(user);
    }



    static class UserViewHolder extends RecyclerView.ViewHolder implements MainActivity.OnImageSelectedListener {

        private final ItemUserBinding binding;
        private final UserListViewModel viewModel; // Reference to ViewModel

        private ActivityResultLauncher<Intent> imagePickerLauncher;

        private final UserListFragment fragment;

        private Uri imageUri;
        private String fileName;

        public UserViewHolder(@NonNull ItemUserBinding binding, UserListViewModel viewModel, ActivityResultLauncher<Intent> imagePickerLauncher, UserListFragment fragment) {
            super(binding.getRoot());
            this.binding = binding;
            this.viewModel = viewModel;
            this.imagePickerLauncher = imagePickerLauncher; // UserAdapter -> UserListFragment -> MainActivity -> returns ActivityResultLauncher
            this.fragment = fragment;
        }



        @Override
        public void onImageSelected(Uri selectedImageUri) {
            if (selectedImageUri != null) {
                imageUri = selectedImageUri;
                Log.d("UserAdapter", "onImageSelected: we go image: " + selectedImageUri);
            }
        }

        public void bind(@NonNull User user) {

            // Get the ActivityResultLauncher from MainActivity
            imagePickerLauncher = ((MainActivity) fragment.requireActivity()).getImagePickerLauncher();

            ((MainActivity) fragment.requireActivity()).setOnImageSelectedListener(this);

            // Change the user image on edit mode
            binding.avatarImage.setOnClickListener(v -> {
                if (binding.btnSave.getVisibility() == View.VISIBLE) {
                    Intent intent = new Intent(Intent.ACTION_PICK, MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
                    imagePickerLauncher.launch(intent);
                }
            });

            // Reset UI to default state
            setEditMode(false);

            binding.nameInput.setText(user.getFirstName() + " " + user.getLastName());
            binding.emailInput.setText(user.getEmail());

            String avatarPath = user.getAvatar();

            if (avatarPath.startsWith("http")) { // Load image from web
                Picasso.get()
                        .load(user.getAvatar())
                        .placeholder(R.drawable.user_placeholder)
                        .into(binding.avatarImage);

                Picasso.get()
                        .load(user.getAvatar())
                        .transform(new BlurTransformation(itemView.getContext(), 25))
                        .into(binding.avatarBlurImage);

            } else { // Load image from internal storage
                File imgFile = new File(itemView.getContext().getFilesDir(), avatarPath);

                if (imgFile.exists()) {
                    Picasso.get()
                            .load(imgFile)
                            .resize(256, 256)
                            .centerCrop()
                            .onlyScaleDown() // ensures that images are only resized if they are larger than 128x128
                            .placeholder(R.drawable.user_placeholder)
                            .into(binding.avatarImage);

                    Picasso.get()
                            .load(imgFile)
                            .resize(256, 256)
                            .onlyScaleDown()
                            .transform(new BlurTransformation(itemView.getContext(), 25))
                            .into(binding.avatarBlurImage);
                } else {
                    binding.avatarImage.setImageResource(R.drawable.user_placeholder);
                }
            }

            binding.avatarImage.setOnClickListener(v -> {
                if (binding.btnSave.getVisibility() == View.VISIBLE) {

                }
            });

            // Set click listener for remove button
            binding.btnRemove.setOnClickListener(v -> showConfirmationDialog(binding.getRoot().getContext(), user));

            // Set click listener for edit button
            binding.editButton.setOnClickListener(v -> setEditMode(true));

            // Set click listener for cancel button
            binding.btnCancel.setOnClickListener(v -> setEditMode(false));

            // Set click listener for save button
            binding.btnSave.setOnClickListener(v -> {
                String name = binding.editTxtName.getText().toString().trim();
                String email = binding.editTxtEmail.getText().toString().trim();

                if (ValidationUtils.validateName(name) && ValidationUtils.validateEmail(email)) {
                    String[] nameParts = name.split(" ", 2);
                    String firstName = ValidationUtils.capitalizeName(nameParts[0]);
                    String lastName = ValidationUtils.capitalizeName(nameParts[1]);

                    user.setFirstName(firstName);
                    user.setLastName(lastName);
                    user.setEmail(email);

                    viewModel.updateUser(user);
                    setEditMode(false);
                } else {
                    if (!ValidationUtils.validateName(name)) {
                        binding.editTxtName.setError("Enter first and last name");
                    }
                    if (!ValidationUtils.validateEmail(email)) {
                        binding.editTxtEmail.setError("Invalid email");
                    }
                }
            });

            // Handle "Next" button press on the keyboard
            binding.editTxtEmail.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_NEXT) {

                    // Hide the keyboard
                    InputMethodManager imm = (InputMethodManager) itemView.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);

                    // Move focus to the next focusable view or handle it as needed
                    if (binding.btnSave.isFocusable()) {
                        binding.btnSave.requestFocus();
                    }
                    return true;
                }
                return false;
            });
        }


        private void setEditMode(boolean isEditing) {
            if (isEditing) {
                binding.editButton.setVisibility(View.GONE);
                binding.editTxt.setVisibility(View.GONE);
                binding.nameInput.setVisibility(View.GONE);
                binding.emailInput.setVisibility(View.GONE);


                binding.editTxtName.setVisibility(View.VISIBLE);
                binding.editTxtEmail.setVisibility(View.VISIBLE);
                binding.btnSave.setVisibility(View.VISIBLE);
                binding.btnCancel.setVisibility(View.VISIBLE);
                binding.btnRemove.setVisibility(View.VISIBLE);

                binding.editTxtName.setText(binding.nameInput.getText());
                binding.editTxtEmail.setText(binding.emailInput.getText());
                binding.editTxtName.requestFocus(); // Request focus for EditText

                // Show keyboard automatically
                binding.editTxtName.postDelayed(() -> {
                    InputMethodManager imm = (InputMethodManager) itemView.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                    imm.showSoftInput(binding.editTxtName, InputMethodManager.SHOW_IMPLICIT);
                }, 200);

            } else {
                binding.editButton.setVisibility(View.VISIBLE);
                binding.editTxt.setVisibility(View.VISIBLE);
                binding.nameInput.setVisibility(View.VISIBLE);
                binding.emailInput.setVisibility(View.VISIBLE);

                binding.editTxtName.setVisibility(View.GONE);
                binding.editTxtEmail.setVisibility(View.GONE);
                binding.btnSave.setVisibility(View.GONE);
                binding.btnCancel.setVisibility(View.GONE);
                binding.btnRemove.setVisibility(View.GONE);
            }
        }

        private void showConfirmationDialog(Context context, User user) {
            AlertDialog.Builder builder = new AlertDialog.Builder(context);
            builder.setTitle("Delete User")
                    .setMessage("Are you sure you want to delete: " + user.getFirstName() + " " + user.getLastName() + "?")
                    .setPositiveButton("Yes", (dialog, which) -> {
                        viewModel.removeUser(user);
                    })
                    .setNegativeButton("No", (dialog, which) -> dialog.dismiss())
                    .create()
                    .show();
        }
    }
}
//    @Override
//    public void onImageSelected(Uri selectedImageUri) {
//        if (selectedImageUri != null) {
//                imageUri = selectedImageUri;
//
//                //Create a unique file name for the image
//                fileName = "user_image_" + System.currentTimeMillis() + ".jpg";
//
//                //Save the file path as the avatar in the user entity
//                binding.avatarImage.setImageURI(imageUri);
//                binding.avatarImage.setTag(fileName); // Store the image URI as a tag
//
//        }
//    }

//    @Override
//    public void onImageSelected(Uri selectedImageUri) {
//        if (selectedImageUri != null) {
//            // Assuming you are updating a specific user's avatar
//            User user = getUserToUpdate(); // Get the user you want to update
//            if (user != null) {
//                String fileName = "user_image_" + System.currentTimeMillis() + ".jpg";
//
//                // Save the new image to internal storage
//                ImageUtils.saveImageToInternalStorage(context, selectedImageUri, fileName);
//
//                // Remove the previous image if it exists
//                String previousImageFileName = user.getAvatar(); // Get the previous image file name
//                if (previousImageFileName != null) {
//                    ImageUtils.removeImageFromStorage(context, previousImageFileName);
//                }
//
//                // Update the user's avatar
//                user.setAvatar(fileName);
//                notifyDataSetChanged(); // Refresh the adapter to show the updated image
//            }
//        }
//     }
