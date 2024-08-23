package natanel.android.crudapp.ui;

import android.content.SharedPreferences;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.database.repository.UserRepository;
import natanel.android.crudapp.service.ApiService;
import natanel.android.crudapp.service.model.UserResponse;
import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class UserListViewModel extends ViewModel {
    private final MutableLiveData<List<User>> _users = new MutableLiveData<>(new ArrayList<>());
    public LiveData<List<User>> users = _users;

    private final MutableLiveData<Boolean> _isLoading = new MutableLiveData<>(false);
    public LiveData<Boolean> isLoading = _isLoading;
    private final MutableLiveData<Integer> currentPage = new MutableLiveData<>(1);
    //private final Executor executor = Executors.newSingleThreadExecutor();
    private static final int NOT_EXISTS = -1;

    private final UserRepository userRepository;
    private final SharedPreferences sharedPreferences;

    public UserListViewModel(UserRepository userRepository, SharedPreferences sharedPreferences) {
        if (userRepository == null) {
            Log.e("UserListViewModel", "UserRepository is null");
        }
        if (sharedPreferences == null) {
            Log.e("UserListViewModel", "SharedPreferences is null");
        }
        this.userRepository = userRepository;
        this.sharedPreferences = sharedPreferences;
        // Any background tasks or initializations can be placed here
        // Initialize data fetch
        //executor.execute(this::getUsers);// Initialize data fetch
        getUsers();
    }


    public void getUsers() {
        int totalApiPages = sharedPreferences.getInt("totalApiPages", NOT_EXISTS);
        int loadedPages = sharedPreferences.getInt("LoadedPages", 1);

        if (Boolean.TRUE.equals(_isLoading.getValue())) return;
        if (currentPage.getValue() == null) return;
        if (currentPage.getValue() > totalApiPages && totalApiPages != -1 ) return;

        _isLoading.setValue(true);

        // On first run, sync data from API. Otherwise, if the current page is within the total API pages:
        //  - If the page is already loaded, update the UI from database.
        //  - If the page is not loaded yet, fetch data from the API, update the database, and then update the UI.
        if (totalApiPages == NOT_EXISTS) {
            syncDataFromApi(currentPage.getValue());
        } else {
                if (currentPage.getValue() <= loadedPages) {
                    observeAndAppendNextPageUsers(currentPage.getValue()); // Update UI
                } else {
                    syncDataFromApi(currentPage.getValue());
                }
            }
    }

    public void loadNextPage() {
        int totalApiPages = sharedPreferences.getInt("totalApiPages", NOT_EXISTS);
        int current = currentPage.getValue() != null ? currentPage.getValue() : 1;

        Log.d("UserListViewModel", "Loading next page: " + current);

        // If the current page is less than the total number of pages and no data is currently being loaded:
        if (current < totalApiPages && Boolean.FALSE.equals(_isLoading.getValue())) {
            currentPage.setValue(current + 1); // Increment current page
            getUsers(); // Fetch users for the next page
        }
    }

    // Update LoadedPages value
    public void updateLoadedPages(int newValue) {
        SharedPreferences.Editor editor = sharedPreferences.edit();
        editor.putInt("LoadedPages", newValue);
        editor.apply();
    }

    // Update TotalApiPages value
    public void updateTotalApiPages(int newValue) {
        SharedPreferences.Editor editor = sharedPreferences.edit();
        editor.putInt("totalApiPages", newValue);
        editor.apply();
    }

    // Appends users from the requested page to the current list and updates the UI.
    private void observeAndAppendNextPageUsers(int page) {
        LiveData<List<User>> getNextUsersLiveData = userRepository.getPaginatedUsers(page);
        Observer<List<User>> observer = new Observer<List<User>>() {
            @Override
            public void onChanged(List<User> users) {
                if (users != null) {
                    List<User> currentUsers = new ArrayList<>(Objects.requireNonNull(_users.getValue()));
                    // Ensure that only new users are added
                    for (User user : users) {
                        if (!currentUsers.contains(user)) {
                            currentUsers.add(user);
                        }
                    }
                    _users.setValue(currentUsers);
                }
                getNextUsersLiveData.removeObserver(this);
            }
        };
        getNextUsersLiveData.observeForever(observer);
    }

    // Syncs data from the API, saves it to the database, and updates the UI.
    public void syncDataFromApi(int page) {
        ApiService.create().getUsers(page).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(@NonNull Call<UserResponse> call, @NonNull Response<UserResponse> response) {
                if (response.isSuccessful() && response.body() != null) {
                    UserResponse userResponse = response.body();
                    userRepository.updateDao(userResponse.getData()); // update the database

                    updateLoadedPages(userResponse.getPage()); // update LoadedPages
                    updateTotalApiPages(userResponse.getTotalPages()); // update TotalApiPages

                    observeAndAppendNextPageUsers(page);
                    _isLoading.setValue(false);
                }
            }

            @Override
            public void onFailure(@NonNull Call<UserResponse> call, Throwable t) {
                _isLoading.setValue(false);
                // Handle error
            }
        });
    }
}
