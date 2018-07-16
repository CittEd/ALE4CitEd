package it.cnr.istc.lecture;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;

import java.util.concurrent.ExecutionException;

public class NavigatorActivity extends AppCompatActivity {

    public static final String TAG = "NavigatorActivity";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
        try {
            Intent intent;
            if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)) && LECTurEContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null))) {
                intent = new Intent(this, MainActivity.class);
            } else {
                intent = new Intent(this, LoginActivity.class);
            }
            startActivity(intent);
        } catch (ExecutionException | InterruptedException e) {
            Log.e(TAG, "login failed..", e);
        }

        finish();
    }
}
