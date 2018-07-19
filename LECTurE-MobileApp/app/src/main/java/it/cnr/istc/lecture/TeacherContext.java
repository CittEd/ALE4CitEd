package it.cnr.istc.lecture;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.User;

public class TeacherContext {

    private final User teacher;
    private boolean on_line;
    /**
     * The current student's parameter types.
     */
    private final List<Parameter> par_types = new ArrayList<>();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current student's parameter values.
     */
    private final Map<String, Map<String, String>> par_vals = new HashMap<>();
    private final List<TeacherListener> listeners = new ArrayList<>();

    TeacherContext(User teacher) {
        this.teacher = teacher;
        this.on_line = teacher.online;
    }

    public User getTeacher() {
        return teacher;
    }

    public boolean isOnLine() {
        return on_line;
    }

    public void setOnLine(boolean on_line) {
        if (this.on_line != on_line) {
            this.on_line = on_line;
            for (TeacherListener l : listeners) l.onlineChanged(on_line);
        }
    }

    public void addListener(TeacherListener l) {
        listeners.add(l);
    }

    public void removeListener(TeacherListener l) {
        listeners.remove(l);
    }

    public interface TeacherListener {

        void onlineChanged(boolean on_line);
    }
}
