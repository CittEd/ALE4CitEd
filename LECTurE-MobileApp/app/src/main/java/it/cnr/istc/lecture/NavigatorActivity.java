package it.cnr.istc.lecture;

import android.Manifest;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;

import java.util.concurrent.ExecutionException;

public class NavigatorActivity extends AppCompatActivity {

    public static final String TAG = "NavigatorActivity";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
        if (!shared_prefs.contains(getString(R.string.email)) || !shared_prefs.contains(getString(R.string.password))) {
            startActivity(new Intent(this, LoginActivity.class));
            finish();
        } else if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, LECTurEContext.ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS);
        } else {
            try {
                if (LECTurEContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null))) {
                    startActivity(new Intent(this, MainActivity.class));
                } else {
                    startActivity(new Intent(this, LoginActivity.class));
                }
                finish();
            } catch (ExecutionException | InterruptedException e) {
                Log.e(TAG, "login failed..", e);
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case LECTurEContext.ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS: {
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
                    try {
                        if (LECTurEContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null))) {
                            startActivity(new Intent(this, MainActivity.class));
                        } else {
                            startActivity(new Intent(this, LoginActivity.class));
                        }
                        finish();
                    } catch (ExecutionException | InterruptedException e) {
                        Log.e(TAG, "login failed..", e);
                    }
                } else
                    finish();
            }
        }
    }
}
