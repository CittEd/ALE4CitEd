package it.cnr.istc.lecture;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;

public class StudentsFragment extends Fragment {

    private RecyclerView students_recycler_view;
    private RecyclerView.Adapter students_adapter;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_students, container, false);

        students_recycler_view = view.findViewById(R.id.students_recycler_view);
        students_adapter = new StudentsAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        students_recycler_view.setHasFixedSize(true);
        students_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        students_recycler_view.setAdapter(students_adapter);

        return view;
    }

    private static class StudentsAdapter extends RecyclerView.Adapter<StudentView> {

        private final List<StudentContext> students = new ArrayList<>();

        @NonNull
        @Override
        public StudentView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new StudentView(LayoutInflater.from(parent.getContext()).inflate(R.layout.student_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull StudentView holder, int position) {
            StudentContext student = students.get(position);
            holder.title.setText(student.getStudent().first_name + " " + student.getStudent().last_name);
        }

        @Override
        public int getItemCount() {
            return students.size();
        }
    }

    private static class StudentView extends RecyclerView.ViewHolder {

        public TextView title;

        public StudentView(View view) {
            super(view);
            title = view.findViewById(R.id.student_name);
        }
    }
}
