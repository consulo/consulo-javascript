/*
 * Copyright 2013-2014 must-be.org
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

package consulo.javascript.debugger;

import consulo.language.editor.FileColorManager;
import consulo.navigation.OpenFileDescriptorFactory;
import consulo.project.Project;
import consulo.ui.ex.awt.ColoredListCellRenderer;
import consulo.ui.ex.awt.JBList;
import consulo.ui.ex.awt.JBScrollPane;
import consulo.ui.ex.awt.SortedListModel;
import consulo.util.io.FileUtil;
import consulo.virtualFileSystem.VirtualFile;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Comparator;

/**
 * @author VISTALL
 * @since 21.03.14
 */
public abstract class JavaScriptListPanel<T> extends JPanel {
    @Nonnull
    private final Project myProject;
    private SortedListModel<T> myModel = SortedListModel.create(new Comparator<T>() {
        @Override
        public int compare(T o1, T o2) {
            VirtualFile v1 = toVirtualFile(o1, false);
            VirtualFile v2 = toVirtualFile(o2, false);
            if (v1 == null || v2 == null) {
                return -1;
            }
            return FileUtil.comparePaths(v1.getPath(), v2.getPath());
        }
    });

    public JavaScriptListPanel(@Nonnull Project project) {
        super(new BorderLayout());
        init();
        myProject = project;
    }

    private void init() {
        final JBList<T> jbList = new JBList<>(myModel);
        jbList.setCellRenderer(new ColoredListCellRenderer<T>() {
            @Override
            protected void customizeCellRenderer(@Nonnull JList<? extends T> jList, T t, int i, boolean b, boolean b1) {
                VirtualFile virtualFile = toVirtualFile(t, false);
                if (virtualFile == null) {
                    append("<invalid>");
                }
                else {
                    setBackground(FileColorManager.getInstance(myProject).getFileColor(virtualFile));
                    append(virtualFile.getPath());
                    setIcon(virtualFile.getFileType().getIcon());
                }
            }
        });
        jbList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2 && e.getButton() == MouseEvent.BUTTON1) {
                    T selectedValue = jbList.getSelectedValue();
                    if (selectedValue == null) {
                        return;
                    }

                    VirtualFile virtualFile = toVirtualFile(selectedValue, true);

                    if (virtualFile != null) {
                        OpenFileDescriptorFactory.getInstance(myProject).builder(virtualFile).build().navigate(true);
                    }
                }
            }
        });

        add(new JBScrollPane(jbList), BorderLayout.CENTER);
    }

    @Nullable
    public abstract VirtualFile toVirtualFile(@Nonnull T value, boolean toOpen);

    public void add(T value) {
        myModel.add(value);
    }
}
