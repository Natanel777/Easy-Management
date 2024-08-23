package natanel.android.crudapp.service.model;

import com.google.gson.annotations.SerializedName;
import java.util.List;

public class UserResponse {
    private List<Data> data;
    private int page;

    @SerializedName("per_page")
    private int perPage;

    private Support support;
    private int total;

    @SerializedName("total_pages")
    private int totalPages;







    // Getters and setters
    public List<Data> getData() {
        return data;
    }

    public void setData(List<Data> data) {
        this.data = data;
    }

    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }

    public int getPerPage() {
        return perPage;
    }

    public void setPerPage(int perPage) {
        this.perPage = perPage;
    }

    public Support getSupport() {
        return support;
    }

    public void setSupport(Support support) {
        this.support = support;
    }

    public int getTotal() {
        return total;
    }

    public void setTotal(int total) {
        this.total = total;
    }

    public int getTotalPages() {
        return totalPages;
    }

    public void setTotalPages(int totalPages) {
        this.totalPages = totalPages;
    }

    @Override
    public String toString() {
        return "UserResponse{" +
                "data=" + data +
                ", page=" + page +
                ", perPage=" + perPage +
                ", support=" + support +
                ", total=" + total +
                ", totalPages=" + totalPages +
                '}';
    }
}
