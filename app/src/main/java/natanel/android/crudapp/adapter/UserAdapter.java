package natanel.android.crudapp.adapter;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.provider.MediaStore;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.ImageView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import de.hdodenhof.circleimageview.CircleImageView;
import jp.wasabeef.picasso.transformations.BlurTransformation;
import natanel.android.crudapp.R;
import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.databinding.ItemUserBinding;
import natanel.android.crudapp.ui.UserListViewModel;
import natanel.android.crudapp.utils.ImageUtils;
import natanel.android.crudapp.utils.ValidationUtils;

public class UserAdapter extends RecyclerView.Adapter<UserAdapter.UserViewHolder> {
    private List<User> users = new ArrayList<>();
    private final UserListViewModel viewModel;
    private final ActivityResultLauncher<Intent> imagePickerLauncher;
    private int currentlyEditingPosition = -1; // -1 means no user is in edit mode

    public UserAdapter(UserListViewModel viewModel, ActivityResultLauncher<Intent> imagePickerLauncher) {
        this.viewModel = viewModel;
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
        return new UserViewHolder(binding, imagePickerLauncher);
    }

    @Override
    public int getItemCount() {
        return users.size();
    }

    @Override
    public void onBindViewHolder(@NonNull UserViewHolder holder, int position) {
        User user = users.get(position);
        holder.bind(user);

        // Ensure no ViewHolder stays in edit mode when bound
        if (currentlyEditingPosition != position) {
            holder.setEditMode(false);
        }

        // Set up an observer for each ViewHolder
        holder.observeUserUpdates(user);
    }

     public class UserViewHolder extends RecyclerView.ViewHolder {
        private final ItemUserBinding binding;
        private final ActivityResultLauncher<Intent> imagePickerLauncher;

        public UserViewHolder(@NonNull ItemUserBinding binding, ActivityResultLauncher<Intent> imagePickerLauncher) {
            super(binding.getRoot());
            this.binding = binding;
            this.imagePickerLauncher = imagePickerLauncher;
        }

        public void bind(@NonNull User user) {
            setEditMode(false);
            binding.nameInput.setText(String.format("%s %s", user.getFirstName(), user.getLastName()));
            binding.emailInput.setText(user.getEmail());
            loadImage(user);

            binding.avatarImage.setOnClickListener(v -> {
                if (binding.btnSave.getVisibility() == View.VISIBLE) {
                    viewModel.setOriginalUserAdapter(user); // save the original user before changes

                    Intent intent = new Intent(Intent.ACTION_PICK, MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
                    imagePickerLauncher.launch(intent);
                }
            });

            // Remove Button
            binding.btnRemove.setOnClickListener(v -> showConfirmationDialog(binding.getRoot().getContext(), user));

            // Edit Button
            binding.editButton.setOnClickListener(v -> setEditMode(true));

            //Cancel Button
            binding.btnCancel.setOnClickListener(v -> {
                User originalUser = viewModel.originalUserAdapter.getValue();
                if (originalUser != null) {
                    loadImage(originalUser);
                }
                setEditMode(false);

                // Remove the observer to prevent multiple updates and Reset props
                viewModel.newUserAdapter.removeObservers((LifecycleOwner) itemView.getContext());
                viewModel.setOriginalUserAdapter(null);
                viewModel.setNewUserAdapter(null);
            });

            //Save Button
            binding.btnSave.setOnClickListener(v -> {
                String name = binding.editTxtName.getText().toString().trim();
                String email = binding.editTxtEmail.getText().toString().trim();

                if (ValidationUtils.validateName(name) && ValidationUtils.validateEmail(email)) {
                    String[] nameParts = name.split(" ", 2);
                    String firstName = ValidationUtils.capitalizeName(nameParts[0]);
                    String lastName = ValidationUtils.capitalizeName(nameParts[1]);
                    User newUser = viewModel.newUserAdapter.getValue();
                    User previousUser = viewModel.originalUserAdapter.getValue();

                    if (newUser != null && previousUser != null) {

                        // Remove old image from internal storage
                        if (previousUser.getAvatar().startsWith("user_image")) {
                            ImageUtils.removeImageFromStorage(itemView.getContext(), previousUser.getAvatar());
                        }

                        // Add new image from internal storage
                        String fileName = "user_image_" + System.currentTimeMillis() + ".jpg";
                        Uri imageUri = Uri.parse(newUser.getAvatar());
                        ImageUtils.saveImageToInternalStorage(itemView.getContext(), imageUri, fileName);
                        user.setAvatar(fileName);
                    }

                    user.setFirstName(firstName);
                    user.setLastName(lastName);
                    user.setEmail(email);

                    viewModel.updateUser(user);
                    setEditMode(false);

                    // Remove the observer to prevent multiple updates and Reset props
                    viewModel.newUserAdapter.removeObservers((LifecycleOwner) itemView.getContext());
                    viewModel.setOriginalUserAdapter(null);
                    viewModel.setNewUserAdapter(null);

                } else {
                    if (!ValidationUtils.validateName(name)) {
                        binding.editTxtName.setError("Enter first and last name");
                    }
                    if (!ValidationUtils.validateEmail(email)) {
                        binding.editTxtEmail.setError("Invalid email");
                    }
                }
            });

            binding.editTxtEmail.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_NEXT) {
                    InputMethodManager imm = (InputMethodManager) itemView.getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                    if (binding.btnSave.isFocusable()) {
                        binding.btnSave.requestFocus();
                    }
                    return true;
                }
                return false;
            });
        }

        public void observeUserUpdates(User user) {
            viewModel.newUserAdapter.observe((LifecycleOwner) itemView.getContext(), newUser -> {
                if (newUser != null && newUser.getId() == user.getId()) {
                    Uri image = Uri.parse(newUser.getAvatar());
                    CircleImageView avatarImage = binding.avatarImage;
                    ImageView blurAvatarImage = binding.avatarBlurImage;

                    Picasso.get()
                            .load(image)
                            .resize(256, 256)
                            .centerCrop()
                            .onlyScaleDown()
                            .placeholder(R.drawable.user_placeholder)
                            .into(avatarImage);

                    Picasso.get()
                            .load(image)
                            .resize(256, 256)
                            .onlyScaleDown()
                            .transform(new BlurTransformation(blurAvatarImage.getContext(), 25))
                            .into(blurAvatarImage);
                }
            });
        }


        private void loadImage(User user) {
            String avatarPath = user.getAvatar();

            // Load image from web or internal storage
            if (avatarPath.startsWith("http")) {
                Picasso.get()
                        .load(user.getAvatar())
                        .placeholder(R.drawable.user_placeholder)
                        .into(binding.avatarImage);

                Picasso.get()
                        .load(user.getAvatar())
                        .transform(new BlurTransformation(itemView.getContext(), 25))
                        .into(binding.avatarBlurImage);
            } else {
                File imgFile = new File(itemView.getContext().getFilesDir(), avatarPath);
                if (imgFile.exists()) {
                    Picasso.get()
                            .load(imgFile)
                            .resize(256, 256)
                            .centerCrop()
                            .onlyScaleDown()
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
        }

        private void showConfirmationDialog(Context context, User user) {
            AlertDialog.Builder builder = new AlertDialog.Builder(context);
            builder.setTitle("Confirm Removal")
                    .setMessage("Are you sure you want to remove this user?")
                    .setPositiveButton("Yes", (dialog, which) -> {

                        // Remove old image from internal storage
                        if (user.getAvatar().startsWith("user_image")) {
                            ImageUtils.removeImageFromStorage(itemView.getContext(), user.getAvatar());
                        }
                        viewModel.removeUser(user);

                        // Remove the observer to prevent multiple updates and Reset props
                        viewModel.newUserAdapter.removeObservers((LifecycleOwner) itemView.getContext());
                        viewModel.setOriginalUserAdapter(null);
                        viewModel.setNewUserAdapter(null);
                    })
                    .setNegativeButton("No", null)
                    .create()
                    .show();
        }

        private void setEditMode(boolean isEditing) {
            Context context = itemView.getContext();
            if (isEditing) {
                // Check if another user is already in edit mode
                Log.d("UserAdapter", "currentlyEditingPosition: " + currentlyEditingPosition + " getAdapterPosition: " + getAdapterPosition());
                if (checkIfThereUserInEdit(context)) return;

                currentlyEditingPosition = getAdapterPosition();
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
                binding.editTxtName.requestFocus();

            } else {
                currentlyEditingPosition = -1; // Reset the position when edit mode is disabled
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

         private boolean checkIfThereUserInEdit(Context context) {
             if (currentlyEditingPosition != -1 && currentlyEditingPosition != getAdapterPosition()) {
                 // Show a toast message
                 Toast.makeText(context, "Finish editing the current user.", Toast.LENGTH_SHORT).show();
                 return true;
             }
             return false;
         }
     }

    @Override
    public void onViewRecycled(@NonNull UserViewHolder holder) {
        super.onViewRecycled(holder);

        // Remove observer when ViewHolder is recycled
        viewModel.newUserAdapter.removeObservers((LifecycleOwner) holder.itemView.getContext());
    }
}
