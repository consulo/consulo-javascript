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

import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.color.ColorValue;
import consulo.ui.ex.awt.ColoredListCellRenderer;
import consulo.ui.ex.awt.JBList;
import consulo.ui.ex.awt.JBScrollPane;
import consulo.ui.ex.awt.SortedListModel;
import consulo.ui.ex.awtUnsafe.TargetAWT;
import consulo.util.io.FileUtil;
import jakarta.annotation.Nonnull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Comparator;

/**
 * @author VISTALL
 * @since 21.03.14
 */
public class JavaScriptListPanel<T extends JavaScriptFileInfo> extends JPanel {
    private SortedListModel<T> myModel = SortedListModel.create((Comparator<T>) (o1, o2) -> FileUtil.comparePaths(o1.getPath(), o2.getPath()));

    public JavaScriptListPanel() {
        super(new BorderLayout());
        init();
    }

    private void init() {
        final JBList<T> list = new JBList<>(myModel);
        list.setCellRenderer(new ColoredListCellRenderer<T>() {
            @Override
            protected void customizeCellRenderer(@Nonnull JList<? extends T> list, T value, int index, boolean selected, boolean hasFocus) {
                append(value.getPath());

                setIcon(value.getIcon());

                if (!selected) {
                    ColorValue fileStatusColor = value.getFileStatusColor();
                    if (fileStatusColor != null) {
                        setBackground(TargetAWT.to(fileStatusColor));
                    }
                }
            }
        });

        list.addMouseListener(new MouseAdapter() {
            @Override
            @RequiredUIAccess
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2 && e.getButton() == MouseEvent.BUTTON1) {
                    T selectedValue = list.getSelectedValue();
                    if (selectedValue == null) {
                        return;
                    }

                    if (selectedValue.canNavigate()) {
                        selectedValue.navigate(true);
                    }
                }
            }
        });

        add(new JBScrollPane(list), BorderLayout.CENTER);
    }

    public void add(T value) {
        myModel.add(value);
    }
}
