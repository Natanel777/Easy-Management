package natanel.android.crudapp.database.repository;

public interface Callback<T> {
    void onComplete(T result);
}
