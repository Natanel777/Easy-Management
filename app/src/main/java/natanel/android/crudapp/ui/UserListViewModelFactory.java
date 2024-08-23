package natanel.android.crudapp.ui;

import android.content.SharedPreferences;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;

import natanel.android.crudapp.database.repository.UserRepository;

public class UserListViewModelFactory implements ViewModelProvider.Factory {
    private final UserRepository userRepository;
    private final SharedPreferences sharedPreferences;

    public UserListViewModelFactory(UserRepository userRepository, SharedPreferences sharedPreferences) {
        this.userRepository = userRepository;
        this.sharedPreferences = sharedPreferences;
    }

    @NonNull
    @Override
    public <T extends ViewModel> T create(@NonNull Class<T> modelClass) {
        if (modelClass.isAssignableFrom(UserListViewModel.class)) {
            return (T) new UserListViewModel(userRepository, sharedPreferences);
        }
        throw new IllegalArgumentException("Unknown ViewModel class");
    }
}
