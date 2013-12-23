/*
 * Copyright 2005-2006 Olivier Descout
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.intellij.idea.lang.javascript.intention.test;

import com.intellij.concurrency.JobScheduler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.TimeUnit;

/**
 */
public abstract class InfoDialog extends JFrame {
    private JLabel       processingLabel;
    private JProgressBar progressBar;
    private JLabel       summaryLabel;
    private int          numCompletedCases;
    private int          numFailedCasesDueToDetection;
    private int          numFailedCasesDueToFix;
    private StringBuffer logBuffer;

    public InfoDialog(int numCases, String title) throws HeadlessException {
        super(title);

        this.setLayout(new BorderLayout());
        this.logBuffer       = new StringBuffer("\n");
        this.processingLabel = new JLabel(" ");
        this.progressBar     = new JProgressBar(JProgressBar.HORIZONTAL, 0, numCases);
        this.summaryLabel    = new JLabel(" ");

        this.getContentPane().add(this.processingLabel, BorderLayout.NORTH);
        this.getContentPane().add(this.progressBar,     BorderLayout.CENTER);
        this.getContentPane().add(this.summaryLabel,    BorderLayout.SOUTH);
        this.pack();

        this.setResizable(false);
        this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new WindowAdapter() {
                @Override
				public void windowClosing(WindowEvent e) {}
            });

        this.showDialog();
        this.setAlwaysOnTop(true);

    }

    protected abstract String getMessage(String key, Object... params);

    private void showDialog() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        final Dimension windowSize = this.getSize();

        // Center Dialogue on screen
        this.setLocation((screenSize.width  - windowSize.width)  / 2,
                         (screenSize.height - windowSize.height) / 2);

        this.setVisible(true);
    }

    @Override
	public Dimension getMinimumSize() {
        FontMetrics metrics = this.processingLabel.getFontMetrics(this.processingLabel.getFont());

        return new Dimension(300, 3 * metrics.getHeight());
    }

    public void setProcessedCaseName(String name) {
        this.processingLabel.setText(this.getMessage("plugin.test.processing", name));
        this.processingLabel.repaint();
    }

    public int getNumCompletedCases() {
        return this.numCompletedCases;
    }

    public int getNumFailedCasesDueToDetection() {
        return this.numFailedCasesDueToDetection;
    }

    public int getNumFailedCasesDueToFix() {
        return this.numFailedCasesDueToFix;
    }

    public void close() {
      JobScheduler.getScheduler().schedule(new Runnable() {
        @Override
		public void run() {
          InfoDialog.this.setVisible(false);
          InfoDialog.this.dispose();
        }
      }, (long)1, TimeUnit.SECONDS);
    }

    public String getLog() {
        return this.logBuffer.append(this.getSummary()).toString();
    }

    private String getSummary() {
        return this.getMessage("plugin.test.summary",
                               this.numCompletedCases,
                               (this.numFailedCasesDueToDetection + this.numFailedCasesDueToFix),
                               this.numFailedCasesDueToDetection,
                               this.numFailedCasesDueToFix);
    }

    public void addCase(String logMessage) {
        this.logBuffer.append(" - ")
                      .append(logMessage)
                      .append('\n');
    }

    public void addCompletedCase(boolean detectionOk, boolean fixOk, String logMessage) {
        this.numCompletedCases++;
        if (!detectionOk) {
            this.numFailedCasesDueToDetection++;
        } else if (!fixOk) {
            this.numFailedCasesDueToFix++;
        }

        this.progressBar.setValue(this.numCompletedCases);
        this.addCase(logMessage);

        this.summaryLabel.setText(this.getSummary());
        this.repaint();
    }
}
