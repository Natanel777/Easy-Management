package natanel.android.crudapp.ui;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import java.util.Objects;

import natanel.android.crudapp.adapter.UserAdapter;
import natanel.android.crudapp.database.repository.UserRepository;
import natanel.android.crudapp.databinding.FragmentUserListBinding;

public class UserListFragment extends Fragment {

    private UserAdapter adapter;
    private FragmentUserListBinding _binding;
    private FragmentUserListBinding getBinding() {
        if (_binding == null) {
            throw new IllegalStateException("Binding should not be null");
        }
        return _binding;
    }

    public static UserListFragment newInstance() {
        return new UserListFragment();
    }


    @Override
    public View onCreateView
            (
            @NonNull LayoutInflater inflater,
            @Nullable ViewGroup container,
            @Nullable Bundle savedInstanceState
    ) {
        _binding = FragmentUserListBinding.inflate(inflater,container,false);
        return getBinding().getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        // Initialize UserRepository and SharedPreferences
        UserRepository userRepository = new UserRepository(getContext());
        SharedPreferences sharedPreferences = requireContext().getSharedPreferences("app_prefs", Context.MODE_PRIVATE);
        UserListViewModelFactory factory = new UserListViewModelFactory(userRepository, sharedPreferences);

        UserListViewModel viewModel = new ViewModelProvider(this, factory).get(UserListViewModel.class);
        //UserListViewModel viewModel = new ViewModelProvider(this).get(UserListViewModel.class);

        Log.d("Data from viewmodel", Objects.requireNonNull(viewModel.users.getValue()).toString());

        adapter = new UserAdapter();

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

        // Optionally observe loading state
        viewModel.isLoading.observe(getViewLifecycleOwner(), isLoading -> {
            // Handle loading state (e.g., show/hide a progress bar)
                getBinding().progressBar.setVisibility(isLoading ? View.VISIBLE : View.GONE);

        });

        getBinding().btnAddUser.setOnClickListener(v -> {
            View cardCreateUser = getBinding().cardCreateUser;
            if (cardCreateUser.getVisibility() == View.GONE) {
                cardCreateUser.setVisibility(View.VISIBLE);
            } else {
                cardCreateUser.setVisibility(View.GONE);
            }
        });
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        _binding = null;
    }
}