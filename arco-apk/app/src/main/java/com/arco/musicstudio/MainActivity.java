package com.arco.musicstudio;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.webkit.PermissionRequest;
import android.webkit.ValueCallback;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import java.io.IOException;
import java.io.InputStream;

public class MainActivity extends Activity {
    private static final int FILE_CHOOSER_REQUEST = 1001;
    private static final int AUDIO_PERMISSION_REQUEST = 1002;
    private static final String APP_HOST = "appassets.androidplatform.net";
    private static final String START_URL = "https://" + APP_HOST + "/index.html";
    private WebView webView;
    private ValueCallback<Uri[]> fileCallback;
    private PermissionRequest pendingWebPermission;

    @Override public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().setStatusBarColor(Color.rgb(9,10,12));
        getWindow().setNavigationBarColor(Color.rgb(9,10,12));
        webView = new WebView(this);
        WebSettings s = webView.getSettings();
        s.setJavaScriptEnabled(true);
        s.setDomStorageEnabled(true);
        s.setDatabaseEnabled(true);
        s.setAllowContentAccess(true);
        s.setAllowFileAccess(true);
        s.setMediaPlaybackRequiresUserGesture(false);
        s.setSupportZoom(true);
        s.setBuiltInZoomControls(true);
        s.setDisplayZoomControls(false);
        s.setMixedContentMode(WebSettings.MIXED_CONTENT_NEVER_ALLOW);
        webView.setBackgroundColor(Color.rgb(9,10,12));

        webView.setWebViewClient(new WebViewClient() {
            private WebResourceResponse asset(String name, String mime, String enc) {
                try { return new WebResourceResponse(mime, enc, getAssets().open(name)); }
                catch (IOException e) { return null; }
            }
            @Override public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest req) {
                Uri u=req.getUrl();
                if(u!=null && "https".equalsIgnoreCase(u.getScheme()) && APP_HOST.equalsIgnoreCase(u.getHost())) {
                    String p=u.getPath();
                    if(p==null || "/".equals(p) || "/index.html".equals(p)) return asset("index.html","text/html","UTF-8");
                    if("/manifest.webmanifest".equals(p)) return asset("manifest.webmanifest","application/manifest+json","UTF-8");
                    if("/sw.js".equals(p)) return asset("sw.js","application/javascript","UTF-8");
                    if(!p.contains(".")) return asset("index.html","text/html","UTF-8");
                }
                return super.shouldInterceptRequest(view,req);
            }
        });

        webView.setWebChromeClient(new WebChromeClient() {
            @Override public void onPermissionRequest(PermissionRequest request) {
                runOnUiThread(() -> {
                    boolean audio=false;
                    for(String r:request.getResources()) if(PermissionRequest.RESOURCE_AUDIO_CAPTURE.equals(r)) audio=true;
                    if(!audio){ request.deny(); return; }
                    if(checkSelfPermission(Manifest.permission.RECORD_AUDIO)==PackageManager.PERMISSION_GRANTED)
                        request.grant(new String[]{PermissionRequest.RESOURCE_AUDIO_CAPTURE});
                    else {
                        pendingWebPermission=request;
                        requestPermissions(new String[]{Manifest.permission.RECORD_AUDIO},AUDIO_PERMISSION_REQUEST);
                    }
                });
            }
            @Override public boolean onShowFileChooser(WebView view, ValueCallback<Uri[]> cb, FileChooserParams p) {
                if(fileCallback!=null) fileCallback.onReceiveValue(null);
                fileCallback=cb;
                try {
                    Intent i=p.createIntent(); i.addCategory(Intent.CATEGORY_OPENABLE);
                    startActivityForResult(i,FILE_CHOOSER_REQUEST); return true;
                } catch(Exception e) { fileCallback=null; return false; }
            }
        });
        setContentView(webView);
        if(savedInstanceState==null) webView.loadUrl(START_URL); else webView.restoreState(savedInstanceState);
    }

    @Override protected void onSaveInstanceState(Bundle out){ if(webView!=null) webView.saveState(out); super.onSaveInstanceState(out); }
    @Override protected void onActivityResult(int requestCode,int resultCode,Intent data){
        super.onActivityResult(requestCode,resultCode,data);
        if(requestCode==FILE_CHOOSER_REQUEST && fileCallback!=null){
            fileCallback.onReceiveValue(WebChromeClient.FileChooserParams.parseResult(resultCode,data));
            fileCallback=null;
        }
    }
    @Override public void onRequestPermissionsResult(int requestCode,String[] permissions,int[] grantResults){
        super.onRequestPermissionsResult(requestCode,permissions,grantResults);
        if(requestCode==AUDIO_PERMISSION_REQUEST && pendingWebPermission!=null){
            if(grantResults.length>0 && grantResults[0]==PackageManager.PERMISSION_GRANTED)
                pendingWebPermission.grant(new String[]{PermissionRequest.RESOURCE_AUDIO_CAPTURE});
            else pendingWebPermission.deny();
            pendingWebPermission=null;
        }
    }
    @Override public void onBackPressed(){ if(webView!=null&&webView.canGoBack()) webView.goBack(); else super.onBackPressed(); }
    @Override protected void onResume(){ super.onResume(); if(webView!=null) webView.onResume(); }
    @Override protected void onPause(){ if(webView!=null) webView.onPause(); super.onPause(); }
    @Override protected void onDestroy(){ if(webView!=null){webView.destroy();webView=null;} super.onDestroy(); }
}