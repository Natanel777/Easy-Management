package natanel.android.crudapp.ui;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import android.provider.MediaStore;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import natanel.android.crudapp.R;
import natanel.android.crudapp.adapter.UserAdapter;
import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.database.repository.UserRepository;
import natanel.android.crudapp.databinding.FragmentUserListBinding;
import natanel.android.crudapp.utils.ImageUtils;
import natanel.android.crudapp.utils.ValidationUtils;

public class UserListFragment extends Fragment {
    private UserAdapter adapter;
    private FragmentUserListBinding _binding;
    private ActivityResultLauncher<Intent> imagePickerLauncher;
    private Uri imageUri;
    private String fileName;

    private FragmentUserListBinding getBinding() {
        if (_binding == null) {
            throw new IllegalStateException("Binding should not be null");
        }
        return _binding;
    }

    @Override
    public View onCreateView
            (
                    @NonNull LayoutInflater inflater,
                    @Nullable ViewGroup container,
                    @Nullable Bundle savedInstanceState
            ) {
        _binding = FragmentUserListBinding.inflate(inflater, container, false);
        return getBinding().getRoot();
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        // Initialize UserRepository and SharedPreferences
        UserRepository userRepository = new UserRepository(getContext());
        SharedPreferences sharedPreferences = requireContext().getSharedPreferences("app_prefs", Context.MODE_PRIVATE);
        UserListViewModelFactory factory = new UserListViewModelFactory(userRepository, sharedPreferences);

        UserListViewModel viewModel = new ViewModelProvider(this, factory).get(UserListViewModel.class);

        // Initialize the ActivityResultLauncher
        imagePickerLauncher = registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                        imageUri = result.getData().getData();
                        if (imageUri != null) {
                            //Create a unique file name for the image
                            fileName = "user_image_" + System.currentTimeMillis() + ".jpg";

                            // Fragment imagePickerLauncher
                            if (viewModel.originalUserAdapter.getValue() == null) {
                                getBinding().imageButton.setImageURI(imageUri);
                                getBinding().imageButton.setTag(fileName);
                            }
                            // Adapter imagePickerLauncher
                            else {
                                User originalUser = viewModel.originalUserAdapter.getValue();
                                // Create a new updated user and copy all values from the original user
                                User updatedUser = new User();
                                updatedUser.setId(originalUser.getId());
                                updatedUser.setFirstName(originalUser.getFirstName());
                                updatedUser.setLastName(originalUser.getLastName());
                                updatedUser.setEmail(originalUser.getEmail());
                                updatedUser.setPosition(originalUser.getPosition());

                                // Update the avatar with the new image URI
                                updatedUser.setAvatar(imageUri.toString());
                                viewModel.setNewUserAdapter(updatedUser);
                            }
                        }
                    }
                }
        );


        // Activating image upload inside MainActivity
        getBinding().imageButton.setOnClickListener(v -> {
            viewModel.setOriginalUserAdapter(null);
            viewModel.setNewUserAdapter(null);

            Intent intent = new Intent(Intent.ACTION_PICK, MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
            imagePickerLauncher.launch(intent);
        });

        adapter = new UserAdapter(viewModel, imagePickerLauncher);
        GridLayoutManager layoutManager = new GridLayoutManager(getContext(), 2);
        getBinding().recycleUsers.setLayoutManager(layoutManager);
        getBinding().recycleUsers.setAdapter(adapter);

        getBinding().recycleUsers.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);

                if (!recyclerView.canScrollVertically(1)) { // Check if reached the bottom
                    viewModel.loadNextPage(); // Load next page
                }
            }
        });

        viewModel.users.observe(getViewLifecycleOwner(), users -> {
            adapter.setUsers(users);
            adapter.notifyDataSetChanged();
        });

        viewModel.isLoading.observe(getViewLifecycleOwner(), isLoading -> getBinding().progressBar.setVisibility(isLoading ? View.VISIBLE : View.GONE));

        // Make add user card visible
        getBinding().btnAddUser.setOnClickListener(v -> {
            View cardCreateUser = getBinding().cardCreateUser;
            if (cardCreateUser.getVisibility() == View.GONE) {
                cardCreateUser.setVisibility(View.VISIBLE);
            } else {
                cardCreateUser.setVisibility(View.GONE);
            }
        });

        getBinding().buttonAdd.setOnClickListener(v -> {
            String fullName = getBinding().txtName.getText().toString().trim();
            String email = getBinding().txtEmail.getText().toString().trim();
            String uriString = (String) getBinding().imageButton.getTag(); // Assuming imageUri is stored as a tag


            if (ValidationUtils.validateName(fullName) && ValidationUtils.validateEmail(email) && uriString != null) {

                // Add image to internal storage
                ImageUtils.saveImageToInternalStorage(requireContext(), imageUri, fileName);

                // Split the name into first and last names
                String[] nameParts = fullName.split(" ", 2);

                // Capitalize the first letter of the first name and last name
                String firstName = nameParts[0].substring(0, 1).toUpperCase() + nameParts[0].substring(1).toLowerCase();
                String lastName = nameParts[1].substring(0, 1).toUpperCase() + nameParts[1].substring(1).toLowerCase();

                userRepository.getLastUserId(count -> {
                    // Create a new User object
                    User newUser = new User();
                    newUser.setId(count + 1); // Make sure to generate a unique ID for the new user
                    newUser.setFirstName(firstName);
                    newUser.setLastName(lastName);
                    newUser.setEmail(email);
                    newUser.setAvatar(uriString); // Convert URI to string for storing
                    newUser.setPosition(1); // Set position for the new user

                    // Add the new user using the repository
                    viewModel.addUser(newUser);

                    // Clear inputs after adding the user
                    requireActivity().runOnUiThread(this::clearInputs);
                });
            } else {
                if (!ValidationUtils.validateName(fullName)) {
                    getBinding().txtName.setError("Enter first and last name");
                }
                if (!ValidationUtils.validateEmail(email)) {
                    getBinding().txtEmail.setError("Invalid email");
                }
                if (uriString == null) {
                    Toast.makeText(getContext(), "Please upload an image", Toast.LENGTH_SHORT).show();
                }
            }
        });
    }

    // Clear input fields after adding the user
    private void clearInputs() {
        getBinding().txtName.setText("");
        getBinding().txtEmail.setText("");
        getBinding().imageButton.setImageResource(R.drawable.ic_placeholder_image); // Set a placeholder image
        getBinding().imageButton.setTag(null); // Clear the image URI
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        _binding = null;
    }
}