package natanel.android.crudapp.ui;

import android.content.SharedPreferences;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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
    private static final int NOT_SET_YET = -1;

    private final UserRepository userRepository;
    private final SharedPreferences sharedPreferences;

    public UserListViewModel(UserRepository userRepository, SharedPreferences sharedPreferences) {

        this.userRepository = userRepository;
        this.sharedPreferences = sharedPreferences;
        getUsers();
    }


    public void getUsers() {
            Log.d("UserListViewModel", "getUsers() Started");
        int totalApiPages = sharedPreferences.getInt("totalApiPages", NOT_SET_YET);
        int loadedPages = sharedPreferences.getInt("LoadedPages", NOT_SET_YET);

        if (Boolean.TRUE.equals(_isLoading.getValue())) return;
        if (currentPage.getValue() == null) return;
        if (currentPage.getValue() > totalApiPages && totalApiPages != -1 ) return;

        _isLoading.postValue(true);

        // On first run, sync data from API. Otherwise, if the current page is within the total API pages:
        //  - If the page is already loaded, update the UI from database.
        //  - If the page is not loaded yet, fetch data from the API, update the database, and then update the UI.
        Log.d("UserListViewModel", "My Current page before doing something drastic is: " + currentPage.getValue());
        if (totalApiPages == NOT_SET_YET) {
            Log.d("UserListViewModel", "Activating syncDataFromApi from getUsers()");
            syncDataFromApi(currentPage.getValue());

        } else {
                if (currentPage.getValue() <= loadedPages) {
                    Log.d("UserListViewModel", "Activating appendNextPageUsers from getUsers()");
                    observeAndAppendNextPageUsers(currentPage.getValue());
                } else {
                    Log.d("UserListViewModel", "Activating syncDataFromApi from getUsers() LAST");
                    syncDataFromApi(currentPage.getValue());
                }
            }
    }



    // Appends users from the requested page to the current list and updates the UI.
    private void observeAndAppendNextPageUsers(int page) {
        LiveData<List<User>> getNextUsersLiveData = userRepository.getPaginatedUsers(page);
        getNextUsersLiveData.observeForever(new Observer<List<User>>() {
            @Override
            public void onChanged(List<User> users) {
                if (users != null && !users.isEmpty()) {
                    List<User> currentUsers = new ArrayList<>(Objects.requireNonNull(_users.getValue()));
                    currentUsers.addAll(users);  // Simply add all users from the new page
                    _users.setValue(currentUsers);
                }
                _isLoading.postValue(false);
                getNextUsersLiveData.removeObserver(this);
            }
        });
    }


    // Syncs data from the API, saves it to the database, and updates the UI.
    public void syncDataFromApi(int page) {
        ApiService.create().getUsers(page).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(@NonNull Call<UserResponse> call, @NonNull Response<UserResponse> response) {
                if (response.isSuccessful() && response.body() != null) {
                    UserResponse userResponse = response.body();
                    userRepository.updateDao(userResponse.getData()); // update the database
                    Log.d("UserListViewModel", "updated database from api with: " + userResponse.getData());

                    updateLoadedPages(userResponse.getPage()); // update LoadedPages
                    updateTotalApiPages(userResponse.getTotalPages()); // update TotalApiPages

                    // Only observe and append after successful API call and update database
                    observeAndAppendNextPageUsers(page);
                } else {
                    _isLoading.postValue(false);
                }
            }

            @Override
            public void onFailure(@NonNull Call<UserResponse> call, @NonNull Throwable t) {
                _isLoading.postValue(false);
                // Handle error
            }
        });
    }

    public void loadNextPage() {
        int totalApiPages = sharedPreferences.getInt("totalApiPages", NOT_SET_YET);
        int current = currentPage.getValue() != null ? currentPage.getValue() : 1;

        // If the current page is less than the total number of pages and no data is currently being loaded:
        if (current < totalApiPages && Boolean.FALSE.equals(_isLoading.getValue())) {
            currentPage.setValue(current + 1); // Increment current page
            Log.d("UserListViewModel", "Loading next page: " + currentPage.getValue());
            getUsers(); // Fetch users for the next page
        }
    }

    // Update LoadedPages value
    public void updateLoadedPages(int newValue) {
        SharedPreferences.Editor editor = sharedPreferences.edit();
        editor.putInt("LoadedPages", newValue);
        editor.apply();
        Log.d("UserListViewModel", "updateLoadedPages LoadedPages is now: " + newValue);
    }

    // Update TotalApiPages value
    public void updateTotalApiPages(int newValue) {
        SharedPreferences.Editor editor = sharedPreferences.edit();
        editor.putInt("totalApiPages", newValue);
        editor.apply();
    }
}
