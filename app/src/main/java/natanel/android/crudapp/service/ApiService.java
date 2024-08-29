package natanel.android.crudapp.service;

import natanel.android.crudapp.service.model.UserResponse;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;
import retrofit2.http.GET;
import retrofit2.http.Query;
import retrofit2.Call;

public interface ApiService {

    @GET("users")
    Call<UserResponse> getUsers(
            @Query("page") int page
    );

    static ApiService create() {
        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl("https://reqres.in/api/")
                .addConverterFactory(GsonConverterFactory.create())
                .build();

        return retrofit.create(ApiService.class);
    }
}

