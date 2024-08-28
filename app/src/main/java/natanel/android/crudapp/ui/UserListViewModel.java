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
        //if (currentPage.getValue() > totalApiPages && totalApiPages != -1 ) return;

        _isLoading.postValue(true);

        // On first run, sync data from API. Otherwise, if the current page is within the total API pages:
        //  - If the page is already loaded, update the UI from database.
        //  - If the page is not loaded yet, fetch data from the API, update the database, and then update the UI.
        Log.d("UserListViewModel", "My Current page before doing something drastic is: " + currentPage.getValue());
        if (totalApiPages == NOT_SET_YET) {
            Log.d("UserListViewModel", "Activating syncDataFromApi from getUsers()");
            syncDataFromApi(currentPage.getValue());

        } else {
            if (currentPage.getValue() <= loadedPages || currentPage.getValue() > totalApiPages) {
                Log.d("UserListViewModel", "Activating appendNextPageUsers from getUsers() current page: " + currentPage.getValue() + " loadedPages: " + loadedPages);
                observeAndAppendNextPageUsers(currentPage.getValue());
            } else {
                Log.d("UserListViewModel", "Activating syncDataFromApi from getUsers() LAST");
                syncDataFromApi(currentPage.getValue());
            }
        }
    }


    // Appends users from the requested page to the current list and updates the UI.
    // We ensure that there are no duplicates by any chance.
    private void observeAndAppendNextPageUsers(int page) {
        LiveData<List<User>> getNextUsersLiveData = userRepository.getPaginatedUsers(page);
        getNextUsersLiveData.observeForever(new Observer<List<User>>() {
            @Override
            public void onChanged(List<User> nextUsers) {
                if (nextUsers != null && !nextUsers.isEmpty()) {
                    List<User> currentUsers = new ArrayList<>(Objects.requireNonNull(_users.getValue()));

                    for (User nextUser : nextUsers) {
                        boolean userExists = false;

                        // Check if the user already exists in the current list
                        for (User existingUser : currentUsers) {
                            if (existingUser.getId() == nextUser.getId()) {
                                userExists = true;
                                break;
                            }
                        }

                        // If the user does not exist, add them to the list
                        if (!userExists) {
                            currentUsers.add(nextUser);
                        }
                    }

                    // Post the updated list
                    _users.setValue(currentUsers);


                } else {
                    // there is no more data available for the next page so adjust the current page number back to the previous page.
                    int current = currentPage.getValue() != null ? currentPage.getValue() : 1;
                    currentPage.setValue(current - 1);
                    Log.d("UserListViewModel", "updating currentPage: " + currentPage.getValue());
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
        if (Boolean.FALSE.equals(_isLoading.getValue())) {
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

    public void addUser(User user) {
        userRepository.addUser(user);

        // Create a new list to hold the updated users
        List<User> currentUsers = new ArrayList<>();
        currentUsers.add(user); // Add the new user first

        // Add existing users from the LiveData
        currentUsers.addAll(Objects.requireNonNull(_users.getValue()));

        // Post the updated list to the LiveData
        _users.postValue(currentUsers);
    }

    public void removeUser(User user) {
        userRepository.deleteUser(user.getId());

        // Update the LiveData to remove the user from the list
        List<User> currentUsers = new ArrayList<>(Objects.requireNonNull(_users.getValue()));
        currentUsers.removeIf(existingUser -> existingUser.getId() == user.getId()); // if exist in the user list remove it
        _users.setValue(currentUsers);
    }

    public void updateUser(User user) {
        userRepository.updateUser(user);

        // Update the LiveData to reflect changes in the UI
        List<User> currentUsers = new ArrayList<>(Objects.requireNonNull(_users.getValue()));
        for (int i = 0; i < currentUsers.size(); i++) {
            if (currentUsers.get(i).getId() == user.getId()) {
                currentUsers.set(i, user);
                break;
            }
        }
        _users.setValue(currentUsers);
    }
}
